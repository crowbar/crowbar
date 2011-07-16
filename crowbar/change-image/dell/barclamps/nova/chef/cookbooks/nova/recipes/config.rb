#
# Cookbook Name:: nova
# Recipe:: config
#
# Copyright 2010, 2011 Opscode, Inc.
# Copyright 2011 Dell, Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

# GREG: Fix this nicely
if node[:crowbar].nil?
  apt_repository "NovaCoreReleasePPA" do
    uri "http://ppa.launchpad.net/nova-core/release/ubuntu"
    distribution node["lsb"]["codename"]
    components ["main"]
    action :add
  end
end

#include_recipe "nova::user"

node[:nova][:my_ip] = Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, "admin").address

package "nova-common" do
  options "--force-yes -o Dpkg::Options::=\"--force-confdef\""
  action :install
end

# GREG: Resolve this nicely
env_filter = " AND nova_config_environment:#{node[:nova][:config][:environment]}"

package "python-mysqldb"
mysqls = search(:node, "recipes:nova\\:\\:mysql#{env_filter}") || []
if mysqls.length > 0
  mysql = mysqls[0]
else
  mysql = node
end
# GREG: Resolve this nicely
mysql_address = mysql[:mysql][:bind_address]
mysql_address = Chef::Recipe::Barclamp::Inventory.get_network_by_type(mysql, "admin").address if mysql_address.nil?
Chef::Log.info("Mysql server found at #{mysql_address}")
sql_connection = "mysql://#{mysql[:nova][:db][:user]}:#{mysql[:nova][:db][:password]}@#{mysql_address}/#{mysql[:nova][:db][:database]}"

rabbits = search(:node, "recipes:nova\\:\\:rabbit#{env_filter}") || []
if rabbits.length > 0
  rabbit = rabbits[0]
else
  rabbit = node
end
# GREG: Resolve this nicely
rabbit_address = rabbit[:rabbitmq][:address]
rabbit_address = Chef::Recipe::Barclamp::Inventory.get_network_by_type(rabbit, "admin").address if rabbit_address.nil? or rabbit_address == "0.0.0.0"
Chef::Log.info("Rabbit server found at #{rabbit_address}")
rabbit_settings = {
  :address => rabbit_address,
  :port => rabbit[:nova][:rabbit][:port],
  :user => rabbit[:nova][:rabbit][:user],
  :password => rabbit[:nova][:rabbit][:password],
  :vhost => rabbit[:nova][:rabbit][:vhost]
}

apis = search(:node, "recipes:nova\\:\\:api#{env_filter}") || []
if apis.length > 0
  api = apis[0]
else
  api = node
end
public_api_ip = api_ip = Chef::Recipe::Barclamp::Inventory.get_network_by_type(api, "public").address
admin_api_ip = api_ip = Chef::Recipe::Barclamp::Inventory.get_network_by_type(api, "admin").address
node[:nova][:api] = public_api_ip
Chef::Log.info("Api server found at #{public_api_ip} #{admin_api_ip}")

# build the public_interface for the fixed net
fixed_net = node["network"]["networks"]["nova_fixed"]
node[:nova][:public_interface] = "br#{fixed_net["vlan"]}"

objectstores = search(:node, "recipes:nova\\:\\:objectstore#{env_filter}") || []
if objectstores.length > 0
  objectstore = objectstores[0]
else
  objectstore = node
end
public_objectstore_ip = Chef::Recipe::Barclamp::Inventory.get_network_by_type(objectstore, "public").address
admin_objectstore_ip = Chef::Recipe::Barclamp::Inventory.get_network_by_type(objectstore, "admin").address
Chef::Log.info("Objectstore server found at #{public_objectstore_ip} #{admin_objectstore_ip}")

networks = search(:node, "recipes:nova\\:\\:network#{env_filter}") || []
if networks.length > 0
  network = networks[0]
else
  network = node
end
network_public_ip = Chef::Recipe::Barclamp::Inventory.get_network_by_type(network, "public").address
Chef::Log.info("Network server found at #{network_public_ip}")

dns_servers = search(:node, "roles:dns-server") || []
if dns_servers.length > 0
  dns_server = dns_servers[0]
else
  dns_server = node
end
dns_server_public_ip = Chef::Recipe::Barclamp::Inventory.get_network_by_type(dns_server, "public").address
Chef::Log.info("DNS server found at #{dns_server_public_ip}")

glance_servers = search(:node, "roles:glance-server") || []
if glance_servers.length > 0
  glance_server = glance_servers[0]
  glance_server_ip = Chef::Recipe::Barclamp::Inventory.get_network_by_type(glance_server, "admin").address
else
  glance_server_ip = nil
end
Chef::Log.info("Glance server at #{glance_server_ip}")


execute "nova-manage db sync" do
  user node[:nova][:user]
  action :nothing
end

cookbook_file "/etc/default/nova-common" do
  source "nova-common"
  owner "root"
  group "root"
  mode 0644
  action :nothing
end

def mask_to_bits(mask)
  octets = mask.split(".")
  count = 0
  octets.each do |octet|
    break if octet == "0"
    c = 1 if octet == "128"
    c = 2 if octet == "192"
    c = 3 if octet == "224"
    c = 4 if octet == "240"
    c = 5 if octet == "248"
    c = 6 if octet == "252"
    c = 7 if octet == "254"
    c = 8 if octet == "255"
    count = count + c
  end

  count
end

fixed_net = node[:network][:networks]["nova_fixed"]
nova_floating = node[:network][:networks]["nova_floating"]
node[:nova][:fixed_range] = "#{fixed_net["subnet"]}/#{mask_to_bits(fixed_net["netmask"])}"
node[:nova][:floating_range] = "#{nova_floating["subnet"]}/#{mask_to_bits(nova_floating["netmask"])}"

if node[:nova][:network_type] == "flat"
  node[:nova][:flat_network][:flat_network_bridge] = "br#{fixed_net["vlan"]}"
elsif node[:nova][:network_type] == "flatdhcp"
  node[:nova][:flat_dhcp_network][:flat_network_bridge] = "br#{fixed_net["vlan"]}"
  node[:nova][:flat_dhcp_network][:flat_network_dhcp_start] = fixed_net["ranges"]["dhcp"]["start"]
elsif node[:nova][:network_type] == "dhcpvlan"
  fixed_interface = Chef::Recipe::Barclamp::Inventory.get_network_by_type(network, "nova_fixed").interface_list.first
  node[:nova][:dhcp_vlan_network][:vlan_interface] = fixed_interface
  node[:nova][:dhcp_vlan_network][:vlan_start] = fixed_net["vlan"] + 1
end

template "/etc/nova/nova.conf" do
  source "nova.conf.erb"
  owner "root"
  group "root"
  mode 0644
  variables(
            :sql_connection => sql_connection,
            :rabbit_settings => rabbit_settings,
            :s3_host => admin_objectstore_ip,
            :s3_dmz_host => public_objectstore_ip,
            :ec2_host => admin_api_ip,
            :ec2_dmz_host => public_api_ip,
            :network_public_ip => network_public_ip,
            :dns_server_public_ip => dns_server_public_ip,
            :glance_server_ip => glance_server_ip
            )
  notifies :run, resources(:execute => "nova-manage db sync"), :immediately
end

package "dnsmasq-base"

