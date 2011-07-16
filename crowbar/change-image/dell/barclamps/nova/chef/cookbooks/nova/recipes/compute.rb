#
# Cookbook Name:: nova
# Recipe:: compute
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

require 'chef/shell_out'

include_recipe "nova::config"

cmd = Chef::ShellOut.new("lsmod")
modules = cmd.run_command
Chef::Log.debug modules

execute "modprobe nbd" do
  action :run
  not_if {modules.stdout.include?("nbd")}
end

package "libvirt-bin" do
  options "--force-yes"
  action :install
end

nova_package("compute")

service "libvirt-bin" do
  action [:enable, :start]
  supports :restart => true, :status => true, :reload => true
  notifies :restart, resources(:service => "nova-compute"), :immediately
  subscribes :restart, resources(:template => "/etc/nova/nova.conf")
end

if node[:nova][:libvirt_type] == "kvm"
  package "kvm"

  execute "modprobe kvm" do
    not_if {modules.stdout.include?("kvm")}
    action :run
    notifies :restart, resources(:service => "libvirt-bin"), :immediately
  end

  execute "chgrp kvm /dev/kvm"

  execute "chmod g+rwx /dev/kvm"
end

# any server that does /NOT/ have nova-api running on it will need this
# firewall rule for UEC images to be able to fetch metadata info
if node[:nova][:api] != node[:nova][:my_ip]
  execute "iptables -t nat -A PREROUTING -d 169.254.169.254/32 -p tcp -m tcp --dport 80 -j DNAT --to-destination 169.254.169.254:8773"
end

