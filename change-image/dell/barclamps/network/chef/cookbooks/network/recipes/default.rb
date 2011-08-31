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

# Walk through a hash of interfaces, adding :order tags to each
# interface that reflects the order in which they should be brought up in.
# They should be torn down in reverse order. 
def sort_interfaces(interfaces)
  seq=0
  i=interfaces.keys.sort.reject{|k| interfaces[k].has_key?(:order)}
  until i.empty?
    i.each do |ifname|
      iface=interfaces[ifname]
      # If this interface has children, and any of its children have not
      # been given a sequence number, skip it.
      next if iface[:interface_list] and not iface[:interface_list].empty? and not iface[:interface_list].all?{|j| interfaces[j].has_key?(:order)}
      iface[:order] = seq
      seq=seq + 1
    end
    i=interfaces.keys.sort.reject{|k| interfaces[k].has_key?(:order)}
  end
  interfaces
end

# Parse the contents of /etc/network/interfaces, and return a data structure
# equivalent to the one we get from Chef.
def local_debian_interfaces
  res={}
  iface=''
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
    when "address" then res[iface][:ipaddress] = parts[1]
    when "netmask" then res[iface][:netmask] = parts[1]
    when "broadcast" then res[iface][:broadcast] = parts[1]
    when "gateway" then res[iface][:router] = parts[1]
    when "bridge_ports" then 
      res[iface][:mode] = "bridge"
      res[iface][:interface_list] = parts[1..-1]
      res[iface][:interface_list].each do |i|
        res[i]=Hash.new unless res[i]
        res[i][:bridge]=iface
      end
    when "vlan_raw_device" then
      res[iface][:mode] = "vlan"
      res[iface][:vlan] = iface.split('.',2)[1].to_i
      res[iface][:interface_list] = Array[ parts[1] ]
    when "down" then 
      res[iface][:mode] = "team"
      res[iface][:interface_list] = parts[4..-1]
      res[iface][:interface_list].each do |i|
        res[i]=Hash.new unless res[i]
        res[i][:master]=iface
        res[i][:slave]=true
      end
    end
  }
  sort_interfaces(res)
end

def local_redhat_interfaces
  res = {}
  ::Dir.entries("/etc/sysconfig/network-scripts").sort.each do |entry|
    next unless entry =~ /^ifcfg/
    next if entry == "ifcfg-lo"
    iface = entry.split('-',2)[1]
    res[iface]=Hash.new unless res[iface]
    ::File.foreach("/etc/sysconfig/network-scripts/#{entry}") do |line|
      line = line.chomp.strip.split('#')[0] # strip comments
      next if line.nil? or ( line.length == 0 ) # skip blank lines
      parts = line.split('=',2)
      k=parts[0]
      v=parts[1]
      case k
      when "DEVICE" then res[iface][:interface]=v
      when "ONBOOT" 
        res[iface][:auto] = true when v == "yes"
      when "BOOTPROTO" then res[iface][:config] = v
      when "IPADDR"
        res[iface][:ipaddress] = v
      when "NETMASK" then res[iface][:netmask] = v
      when "BROADCAST" then res[iface][:broadcast] = v
      when "GATEWAY" then res[iface][:router] = v
      when "MASTER" 
        res[iface][:master] = v
        res[v]=Hash.new unless res[v]
        res[v][:mode] = "team"
        res[v][:interface_list]=Array.new unless res[v][:interface_list]
        res[v][:interface_list].push(iface)
      when "SLAVE"
        res[iface][:slave] = true if v == "yes"
      when "BRIDGE"
        res[iface][:bridge] = v
        res[v]=Hash.new unless res[v]
        res[v][:mode] = "bridge"
        res[v][:interface_list]=Array.new unless res[iface][:interface_list]
        res[v][:interface_list].push(iface)
      when "VLAN"
        res[iface][:mode] = "vlan"
        res[iface][:vlan] = iface.split('.',2)[1].to_i
        res[iface][:interface_list]=[iface.split('.',2)[0]]
      end
    end
    if res[iface][:config] == "none"
      res[iface][:config] = if res[iface][:ipaddress]
                              "static"
                            else
                              "manual"
                            end
    end
    res[iface][:auto] = false unless res[iface][:auto]
  end
  sort_interfaces(res)
end

def crowbar_interfaces
  res = Hash.new
  node["crowbar"]["network"].each do |intf, network|
    next if intf == "bmc"
    res[intf] = Hash.new unless res[intf]
    res[intf][:interface] = intf
    res[intf][:auto] = true
    # Handle vlans first.
    if network["use_vlan"]
      res[intf][:vlan] = network["vlan"]
      res[intf][:mode] = "vlan"
      res[intf][:interface_list] = network["interface_list"].reject{|x| x == intf}
    end
    # If we were asked to make a bridge, do it second.
    if network["add_bridge"]
      # We have to make up a bridge name here
      res[intf][:bridge]=if res[intf][:vlan] 
                           "br#{res[intf][:vlan]}"
                         else
                           "br#{intf}"
                         end
      # That base interface now has a manual config
      res[intf][:config]="manual"
      base_if=intf
      intf=res[intf][:bridge]
      res[intf]=Hash.new unless res[intf]
      res[intf][:interface] = intf
      res[intf][:interface_list] = [ base_if ]
      res[intf][:auto] = true
      res[intf][:mode]="bridge"
    # If we were not asked to make a bridge and have a non-empty interface
    # list, we must have been asked to make a team.
    elsif node["network"]["mode"] == "team" and network["interface_list"] and not network["interface_list"].empty?
      res[intf][:interface_list] = network["interface_list"].reject{|x| x == intf}
      res[intf][:mode]="team"
      # Since we are making a team out of these devices, blow away whatever
      # config we may have had for the slaves.
      res[intf][:interface_list].each do |i|
        res[i]=Hash.new
        res[i][:interface]=i
        res[i][:auto]=false
        res[i][:config]="manual"
        res[i][:slave]=true
        res[i][:master]=intf
      end
    end
    if network["address"] and network["address"] != "0.0.0.0"
      res[intf][:config] = "static"
      res[intf][:ipaddress] = network["address"]
      res[intf][:netmask] = network["netmask"]
      res[intf][:broadcast] = network["broadcast"]
      res[intf][:router] = network["router"]
    else
      res[intf][:config] = "manual"
    end
  end
  sort_interfaces(res)
end

# Make sure that the /etc/network/if-up.d/upstart file is gone
# We manage apache2 (and others), it shouldn't
case node[:platform]
when "ubuntu","debian"
  file "/etc/network/if-up.d/upstart" do
    action :delete
  end
end

package "bridge-utils"

case node[:platform]
when "ubuntu","debian"
  package "vlan"
  package "ifenslave-2.6"
when "centos","redhat"
  package "vconfig"
end

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
old_interfaces = case node[:platform]
                 when "debian","ubuntu"
                   local_debian_interfaces
                 when "centos","redhat"
                   local_redhat_interfaces
                 end
new_interfaces = crowbar_interfaces
interfaces_to_up={}

def deorder(i)
  i.reject{|k,v|k == :order or v.nil? or (v.respond_to?(:empty?) and v.empty?)}
end

log("Current interfaces:\n#{old_interfaces.inspect}\n") { level :debug }
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
    next if i.nil? or i == ""
    log("Removing #{old_interfaces[i]}\n") { level :debug }
    bash "ifdown #{i} for removal" do
      code "ifdown #{i}"
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
        interfaces_to_up[i] = "ifup #{i}"
      else
        # We are giving it a manual config.  Ifdown the interface, and then
        # schedule it to be ifup'ed based on whether or not :auto is true.
        bash "ifdown #{i} for crowbar capture" do
          code "ifdown #{i}"
        end
        interfaces_to_up[i] = "ifup #{i}" if new_interfaces[i][:auto]
        delay = true
      end
    else
      # The interface changed, and it is not a matter of taking ownership
      # from the OS.  ifdown it now, and schedule it to be ifup'ed if 
      # the new config is set to :auto.
      bash "ifdown #{i} for reconfigure" do
        code "ifdown #{i}"
      end
      interfaces_to_up[i] = "ifup #{i}" if new_interfaces[i][:auto]
      delay = true
    end
  }
  
  # Third, rewrite the network configuration to match the new config.
  case node[:platform]
  when "ubuntu","debian"
    template "/etc/network/interfaces" do
      source "interfaces.erb"
      variables :interfaces => new_interfaces.values.sort{|a,b| 
        a[:order] <=> b[:order]
      }
    end
  when "centos","redhat"
    new_interfaces.values.each {|iface|
      template "/etc/sysconfig/network-scripts/ifcfg-#{iface[:interface]}" do
        source "redhat-cfg.erb"
        variables :iface => iface
      end
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
