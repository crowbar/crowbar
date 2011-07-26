# Copyright 2011, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

include_recipe 'utils'

# Parse the contents of /etc/network/interfaces, and return a data structure
# equivalent to the one we get from Chef.
def local_interfaces
  res={}
  iface=''
  order = 0
  File.foreach("/etc/network/interfaces") {|line|
    line = line.chomp.strip.split('#')[0] # strip comments
    next if line.nil? or ( line.length == 0 ) # skip blank lines
    parts = line.split
    case parts[0]
    when "auto"
      parts[1..-1].each { |name|
        next if name == "lo"
        res[name] = Hash.new unless res[name]
        res[name][:auto] = true
        res[name][:interface] = name unless res[name][:interface]
      }
    when "iface"
      iface = parts[1]
      next if iface == "lo"
      res[iface] = Hash.new unless res[iface]
      res[iface][:interface] = iface
      res[iface][:interface_list] = Array.new
      res[iface][:config] = parts[3]
      res[iface][:order] = order
    when "address" then res[iface][:ipaddress] = parts[1]
    when "netmask" then res[iface][:netmask] = parts[1]
    when "broadcast" then res[iface][:broadcast] = parts[1]
    when "gateway" then res[iface][:router] = parts[1]
    when "bridge_ports" then 
      res[iface][:mode] = "bridge"
      res[iface][:interface_list] = parts[1..-1]
    when "vlan_raw_device" then
      res[iface][:mode] = "vlan"
        res[iface][:vlan] = iface.split('.',2)[1].to_i
        res[iface][:interface_list] = Array[ parts[1] ]
    when "down" then 
      res[iface][:mode] = "team"
      res[iface][:interface_list] = parts[4..-1]
    end
    order = order + 1
  }
  res
end

def crowbar_interfaces
  res = Hash.new
  order = 0
  node["crowbar"]["network"].each do |intf, network|
    next if intf == "bmc"
    iface = Hash.new
    if network["add_bridge"]
      # Add base interface
      iface[:interface] = intf
      iface[:order] = order
      order = order + 1
      iface[:config] = "manual"
      iface[:auto] = true
      if network["use_vlan"]
        iface[:vlan] = network["vlan"]
        iface[:mode] = "vlan"
        iface[:interface_list] = network["interface_list"].reject{|x| x == intf}
      elsif network["interface_list"] and not network["interface_list"].empty?
        iface[:interface_list] = network["interface_list"].reject{|x| x == intf}
        iface[:mode] = "team" if iface[:interface_list] and not iface[:interface_list].empty? 
      end
      res[intf] = iface.dup
      # now, build a bridge out of it.
      iface = Hash.new
      # Network interfaces are made of wood, right?  Or was it witches?
      iface[:interface_list] = [ intf ]
      # Definitly wood.
      intf = "br#{network["vlan"]}"
      iface[:interface] = intf
      iface[:mode] = "bridge"
    else
      # Not a bridge, use normal config.
      iface[:interface] = intf
      if network["use_vlan"]
        iface[:vlan] = network["vlan"]
        iface[:mode] = "vlan"
        iface[:interface_list] = network["interface_list"].reject{|x| x == intf}
      elsif network["interface_list"] and not network["interface_list"].empty?
        iface[:interface_list] = network["interface_list"].reject{|x| x == intf}
        iface[:mode] = "team" if iface[:interface_list] and not iface[:interface_list].empty? 
      end
    end
    # Common network config for all interfaces.
    iface[:auto] = true
    iface[:order] = order
    order = order + 1
    if network["address"] and network["address"] != "0.0.0.0"
      iface[:config] = "static"
      iface[:ipaddress] = network["address"]
      iface[:netmask] = network["netmask"]
      iface[:broadcast] = network["broadcast"]
      iface[:router] = network["router"]
    else
      iface[:config] = "manual"
    end
    res[intf] = iface.dup
  end
  res
end

# Make sure that the /etc/network/if-up.d/upstart file is gone
# We manage apache2 (and others), it shouldn't
file "/etc/network/if-up.d/upstart" do
  action :delete
end

package "bridge-utils"
package "vlan"
package "ifenslave-2.6"

utils_line "8021q" do
  action :add
  file "/etc/modules"
end

bash "load 8021q module" do
  code "/sbin/modprobe 8021q"
  not_if { ::File.exists?("/sys/module/8021q") }
end

if node["network"]["mode"] == "team"
  utils_line "bonding mode=6 miimon=100" do
    action :add
    file "/etc/modules"
  end
  
  bash "load bonding module" do
    code "/sbin/modprobe bonding mode=6 miimon=100"
    not_if { ::File.exists?("/sys/module/bonding") }
  end
end

delay = false
old_interfaces = local_interfaces
new_interfaces = crowbar_interfaces
interfaces_to_up={}

def deorder(i)
  i.reject{|k,v|k == :order or v.nil? or (v.respond_to?(:empty?) and v.empty?)}
end

log("Current interfaces:\n#{old_interfaces.inspect}") { level :debug }
log("New interfaces:\n#{new_interfaces.inspect}\n") { level :debug }

if (not new_interfaces) or new_interfaces.empty?
  log("Crowbar instructed us to tear down all our interfaces!") { level :fatal }
  log("Refusing to do so.") { level :fatal }
  raise ::RangeError.new("Not enough active network interfaces.")
else
  # First, tear down any interfaces that are going to be deleted in 
  # reverse order in which they appear in the current /etc/network/interfaces
  (old_interfaces.keys - new_interfaces.keys).sort{|a,b| 
    old_interfaces[b][:order] <=> old_interfaces[a][:order]}.each {|i|
    log("Removing #{old_interfaces[i]}\n") { level :debug }
    bash "ifdown #{i} for removal" do
      code "ifdown --force #{i}"
    end
  }
  
  # Second, examine each interface that exists in both the old and the
  # new configuration to see what changed, and take appropriate action.
  (old_interfaces.keys & new_interfaces.keys).each {|i|
    log("Transitioning #{i}:\n#{old_interfaces[i].inspect}\n=>\n#{new_interfaces[i].inspect}\n") { level :debug }
    case
    when deorder(old_interfaces[i]) == deorder(new_interfaces[i])
      # The only thing that changed is the proposed position in the interfaces
      # file.  Don't do anything with this interface.
      log "#{i} did not change, skipping."
      next
    when old_interfaces[i][:config] == "dhcp"
      # We are going to transition an interface into being owned by Crowbar.
      # Kill any dhclients for this interface, and then take action
      # based on whether we are giving it an IP address or not.
      bash "kill dhclients" do
        code "killall dhclient3"
        only_if "pidof dhclient3"
      end
      if new_interfaces[i][:config] == "static"
        # We are giving it a static IP.  Schedule the interface to be 
        # forced up with the new config, which should give it the new 
        # configuration without taking the link down.
        # We rely on our static network config being otherwise identical
        # to our DHCP config.
        interfaces_to_up[i] = "ifup --force #{i}"
      else
        # We are giving it a manual config.  Ifdown the interface, and then
        # schedule it to be ifup'ed based on whether or not :auto is true.
        bash "ifdown #{i} for crowbar capture" do
          code "ifdown --force #{i}"
        end
        interfaces_to_up[i] = "ifup #{i}" if new_interfaces[i][:auto]
        delay = true
      end
    else
      # The interface changed, and it is not a matter of taking ownership
      # from the OS.  ifdown it now, and schedule it to be ifup'ed if 
      # the new config is set to :auto.
      bash "ifdown #{i} for reconfigure" do
        code "ifdown --force #{i}"
      end
      interfaces_to_up[i] = "ifup #{i}" if new_interfaces[i][:auto]
      delay = true
    end
  }
  
  # Third, rewrite /etc/network/interfaces to make sure our new interfaces
  # are brought up with the correct parameters
  
  template "/etc/network/interfaces" do
    source "interfaces.erb"
    variables :interfaces => new_interfaces.values.sort{|a,b| 
      a[:order] <=> b[:order]
    }
  end
  
  # Fourth, bring up any new or changed interfaces
  new_interfaces.values.sort{|a,b|a[:order] <=> b[:order]}.each {|i|
    next if i[:interface] == "bmc"
    case
    when (old_interfaces[i[:interface]].nil? and i[:auto])
      # This is a new interface.  Ifup it if it should be auto ifuped.
      bash "ifup new #{i[:interface]}" do
        code "ifup #{i[:interface]}"
      end
      delay = true
    when interfaces_to_up[i[:interface]]
      # This is an interface that we had in common with old_interfaces that
      # did not have an identical configuration from the last time.
      # We need to bring it up according to the instructions left behind.
      bash "ifup reconfigured #{i[:interface]}" do
        code interfaces_to_up[i[:interface]]
      end
    end
  }
  
  # If we need to sleep now, do it.
  if delay
    delay_time = node["network"]["start_up_delay"]
    log "Sleeping for #{delay_time} seconds due new link coming up"
    bash "network delay sleep" do
      code "sleep #{delay_time}"
    end
  end
end
