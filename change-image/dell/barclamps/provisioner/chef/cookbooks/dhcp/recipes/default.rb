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

