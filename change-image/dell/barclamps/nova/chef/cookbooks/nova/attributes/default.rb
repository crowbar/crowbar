#
# Cookbook Name:: nova
# Attributes:: default
#
# Copyright 2008-2011, Opscode, Inc.
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

::Chef::Node.send(:include, Opscode::OpenSSL::Password)

#
# Database Settings
#
default[:nova][:db][:password] = "" # Set by Recipe
default[:nova][:db][:user] = "nova"
default[:nova][:db][:database] = "nova"
default[:nova][:mysql] = false

#
# RabbitMQ Settings
#
set_unless[:nova][:rabbit][:password] = secure_password
default[:nova][:rabbit][:user] = "nova"
default[:nova][:rabbit][:vhost] = "/nova"

#
# Hypervisor Settings
#
default[:nova][:libvirt_type] = "kvm"

#
# Shared Settings
#
default[:nova][:hostname] = "nova"
default[:nova][:my_ip] = ipaddress
default[:nova][:api] = ""
default[:nova][:user] = "nova"
default[:nova][:user_group] = "nogroup"
default[:nova][:user_dir] = "/var/lib/nova"
default[:nova][:project] = "admin"
set_unless[:nova][:access_key] = secure_password
set_unless[:nova][:secret_key] = secure_password

#
# General network parameters
#
default[:nova][:network_type] = "flat" # support "flatdhcp "flat" "dhcpvlan"
default[:nova][:public_interface] = "eth0"
default[:nova][:routing_source_ip] = ipaddress
default[:nova][:fixed_range] = "10.0.0.0/8"
default[:nova][:floating_range] = "4.4.4.0/24"
default[:nova][:num_networks] = 1
default[:nova][:network_size] = 256
#
# Flat parameters
#
default[:nova][:flat_network][:flat_network_bridge] = "br100"
default[:nova][:flat_network][:flat_injected] = true
default[:nova][:flat_network][:flat_dns] = "8.8.4.4"
#
# Flat DHCP Parameters
#
default[:nova][:flat_dhcp_network][:flat_network_bridge] = "br100"
default[:nova][:flat_dhcp_network][:flat_dns] = "8.8.4.4"
default[:nova][:flat_dhcp_network][:flat_interface] = "eth0"
default[:nova][:flat_dhcp_network][:flat_network_dhcp_start] = "10.0.0.2"
#
# DHCP Vlan Parameters
#
default[:nova][:dhcp_vlan_network][:vlan_interface] = "eth1"
default[:nova][:dhcp_vlan_network][:vlan_start] = 100
default[:nova][:dhcp_vlan_network][:vpn_start] = 1000
default[:nova][:dhcp_vlan_network][:vpn_ip] = ipaddress

#
# Default images to import
#
default[:nova][:images] = []
