#
# Cookbook Name:: nova
# Recipe:: rabbit
#
# Copyright 2010, Opscode, Inc.
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

# GREG: Resolve this nicely
node[:rabbitmq][:address] = Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, "admin").address
#node[:rabbitmq][:address] = node[:nova][:my_ip]

include_recipe "rabbitmq"

# add a vhost to the queue
rabbitmq_vhost node[:nova][:rabbit][:vhost] do
  action :add
end

# create user for the queue
rabbitmq_user node[:nova][:rabbit][:user] do
  password node[:nova][:rabbit][:password]
  action :add
end

# grant the mapper user the ability to do anything with the vhost
# the three regex's map to config, write, read permissions respectively
rabbitmq_user node[:nova][:rabbit][:user] do
  vhost node[:nova][:rabbit][:vhost]
  permissions "\".*\" \".*\" \".*\""
  action :set_permissions
end

# save data so it can be found by search
node.save

