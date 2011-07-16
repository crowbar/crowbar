
include_recipe 'utils'

nova_network = node[:nova][:flat_interface] if !node[:nova].nil? and !node[:nova][:network_type].nil?
nova_found = false

interfaces = Array.new
if_map = {}
node["crowbar"]["network"].each do |intf, network|
  nova_found = true if !nova_network.nil? and nova_network == intf
  if network["add_bridge"]
    # Add base interface
    interfaces << {
      :interface => intf,
      :ipaddress => "0.0.0.0",
      :vlan => network["vlan"],
      :use_vlan => network["use_vlan"],
      :interface_list => network["interface_list"]
    }

    # Add bridge
    interfaces << {
      :interface => "br#{network["vlan"]}",
      :ipaddress => network["address"],
      :subnet => network["subnet"],
      :netmask => network["netmask"],
      :broadcast => network["broadcast"],
      :router => network["router"],
      :interface_list => [ intf ]
    }

    if_map[intf] = true
    if_map["br#{network["vlan"]}"] = true
  else
    interfaces << {
      :interface => intf,
      :ipaddress => network["address"],
      :subnet => network["subnet"],
      :netmask => network["netmask"],
      :broadcast => network["broadcast"],
      :router => network["router"],
      :vlan => network["vlan"],
      :use_vlan => network["use_vlan"],
      :interface_list => network["interface_list"]
    }
    if_map[intf] = true
  end
end unless node["crowbar"].nil? or node["crowbar"]["network"].nil?

if !nova_network.nil? and !nova_found
  Chef::Log.fatal("Failed to define an interface with IP address for Nova!")
  interfaces << {
    :interface => nova_network
  }
end

links = `ip -o link`.split("\n")
remove_if = []
links.each do |link|
  if link =~ /^[0-9]+: (.*): /
    intf = $1.split("@")[0]
    next if if_map[intf]
    next if intf == "lo"
    next if intf =~ /^vnet/
    if node["crowbar"]["network"][intf].nil?
      remove_if << intf
    end
  end
end

package "bridge-utils"
package "vlan"
package "ifenslave-2.6"

# Make sure that the /etc/network/if-up.d/upstart file is gone
# We manage apache2 (and others), it shouldn't
file "/etc/network/if-up.d/upstart" do
  action :delete
end

utils_line "8021q" do
  action :add
  file "/etc/modules"
end

bash "load 8021q module" do
  code "/sbin/modprobe 8021q"
  not_if "lsmod | grep -q 8021q"
end

if node["network"]["mode"] == "team"
  utils_line "bonding mode=6 miimon=100" do
    action :add
    file "/etc/modules"
  end

  bash "load bonding module" do
    code "/sbin/modprobe bonding mode=6 miimon=100"
    not_if "lsmod | grep -q bonding"
  end
end

bash "ensure no dhclient" do
  code "killall dhclient3"
  only_if "ps -ef | grep -v grep | grep -q dhclient"
end

template "/etc/network/interfaces" do
  source "interfaces.erb"
  variables :interfaces => interfaces
  notifies :run, "execute[restart-networking]", :immediately
end

execute "restart-networking" do
  command "/etc/init.d/networking restart"
  action :nothing
  notifies :run, "execute[delay-networking]", :immediately
end

execute "delay-networking" do
  command "sleep 30"
  action :nothing
end

remove_if = remove_if.sort!{ |a, b| b <=> a }
remove_if.each do |link|
  bash "remove #{link} interface" do
    code "ifconfig #{link} inet 0.0.0.0 ; ip link delete #{link} ; true"
    only_if "ip link show #{link}"
  end
end

if node["network"]["mode"] != "team"
  utils_line "bonding mode=6 miimon=100" do
    action :remove
    file "/etc/modules"
  end

  bash "remove bonding module" do
    code "/sbin/rmmod bonding"
    only_if "lsmod | grep -q bonding"
    notifies :run, "execute[restart-networking]", :immediately
  end
end

