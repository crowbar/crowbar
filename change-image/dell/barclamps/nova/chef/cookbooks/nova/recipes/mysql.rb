#
# Cookbook Name:: nova
# Recipe:: mysql
#
# Copyright 2010-2011, Opscode, Inc.
# Copyright 2011, Dell, Inc.
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

include_recipe "mysql::server"

#nova db user and root have same password
node[:nova][:db][:password] = node[:mysql][:server_root_password]
# GREG: Resolve this nicely.
node[:mysql][:bind_address] = Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, "admin").address
#node[:mysql][:bind_address] = node[:nova][:my_ip]

execute "mysql-install-nova-privileges" do
  command "/usr/bin/mysql -u root -p#{node[:nova][:db][:password]} < /etc/mysql/nova-grants.sql"
  action :nothing
end

# Permissions for the nova user
template "/etc/mysql/nova-grants.sql" do
  path "/etc/mysql/nova-grants.sql"
  source "grants.sql.erb"
  owner "root"
  group "root"
  mode "0600"
  variables(
    :user     => node[:nova][:db][:user],
    :password => node[:nova][:db][:password],
    :database => node[:nova][:db][:database]
  )
  notifies :run, resources(:execute => "mysql-install-nova-privileges"), :immediately
end

# Creates empty nova database
mysql_database "create #{node[:nova][:db][:database]} database" do
  host "localhost"
  username "root"
  password node[:nova][:db][:password]
  database node[:nova][:db][:database]
  action :create_db
end

# save data so it can be found by search
node.save
