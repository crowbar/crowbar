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

include_recipe "utils"

pkg = ""
case node[:platform]
when "ubunu","debian"
  pkg = "dhcp3"
  package "dhcp3-server"
when "redhat","centos"
  pkg = "dhcp"
  package "dhcp"
end

directory "/etc/dhcp3"
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

service "dhcp3-server" do
  service_name "dhcpd" if node[:platform] =~ /^(redhat|centos)$/
  supports :restart => true, :status => true, :reload => true
  action :enable
end

# This needs to be evaled.
intfs = [ Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, "admin").interface ]

case node[:platform]
when "ubuntu","debian"
  template "/etc/dhcp3/dhcpd.conf" do
    owner "root"
    group "root"
    mode 0644
    source "dhcpd.conf.erb"
    variables(:options => node[:dhcp][:options])
    notifies :restart, "service[dhcp3-server]"
  end

  template "/etc/default/dhcp3-server" do
    owner "root"
    group "root"
    mode 0644
    source "dhcp3-server.erb"
    variables(:interfaces => intfs)
    notifies :restart, "service[dhcp3-server]"
  end
when "redhat","centos"
  template "/etc/dhcpd.conf" do
    owner "root"
    group "root"
    mode 0644
    source "dhcpd.conf.erb"
    variables(:options => node[:dhcp][:options])
    notifies :restart, "service[dhcp3-server]"
  end

  template "/etc/sysconfig/dhcpd" do
    owner "root"
    group "root"
    mode 0644
    source "redhat-sysconfig-dhcpd.erb"
    variables(:interfaces => intfs)
    notifies :restart, "service[dhcp3-server]"
  end
end

