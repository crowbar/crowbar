
include_recipe "utils"

package "dhcp3-server"

directory "/etc/dhcp3/groups.d"
directory "/etc/dhcp3/subnets.d"
directory "/etc/dhcp3/hosts.d"

file "/etc/dhcp3/groups.d/group_list.conf" do
  owner "root"
  group "root"
  mode 0644
end
file "/etc/dhcp3/subnets.d/subnet_list.conf" do
  owner "root"
  group "root"
  mode 0644
end
file "/etc/dhcp3/hosts.d/host_list.conf" do
  owner "root"
  group "root"
  mode 0644
end

template "/etc/dhcp3/dhcpd.conf" do
  owner "root"
  group "root"
  mode 0644
  source "dhcpd.conf.erb"
  variables(:options => node[:dhcp][:options])
end

# This needs to be evaled.
intfs = [ Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, "admin").interface ]

template "/etc/default/dhcp3-server" do
  owner "root"
  group "root"
  mode 0644
  source "dhcp3-server.erb"
  variables(:interfaces => intfs)
end

service "dhcp3-server" do
  supports :restart => true, :status => true, :reload => true
  enabled
  action :nothing
end

