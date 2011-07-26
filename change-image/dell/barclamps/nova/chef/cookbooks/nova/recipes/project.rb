#
# Cookbook Name:: nova
# Recipe:: setup
#
# Copyright 2010-2011, Opscode, Inc.
# Copyright 2011, Anso Labs
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

package "euca2ools"
package "unzip"

# Project
execute "nova-manage user admin #{node[:nova][:user]} #{node[:nova][:access_key]} #{node[:nova][:secret_key]}" do
  user node[:nova][:user]
  not_if "nova-manage user list | grep #{node[:nova][:user]}"
end

execute "nova-manage project create #{node[:nova][:project]} #{node[:nova][:user]}" do
  user node[:nova][:user]
  not_if "nova-manage project list | grep #{node[:nova][:project]}"
end

execute "nova-manage network create #{node[:nova][:fixed_range]} #{node[:nova][:num_networks]} #{node[:nova][:network_size]}" do
  user node[:nova][:user]
  not_if { ::File.exists?("/var/lib/nova/setup") }
end

if node[:nova][:network_type] == "dhcpvlan"
  execute "nova-manage floating create #{node[:nova][:hostname]} #{node[:nova][:floating_range]}" do
    user node[:nova][:user]
    not_if { ::File.exists?("/var/lib/nova/setup") }
  end
end

if node[:nova][:network_type] != "dhcpvlan"
  execute "mysql-fix-ranges-fixed" do
    command "/usr/bin/mysql -u root -p#{node[:mysql][:server_root_password]} #{node[:nova][:db][:database]} < /etc/mysql/nova-fixed-range.sql"
    action :nothing
  end

  fixed_net = node[:network][:networks]["nova_fixed"]
  rangeH = fixed_net["ranges"]["dhcp"]
  netmask = fixed_net["netmask"]
  subnet = fixed_net["subnet"]

  index = IPAddr.new(rangeH["start"]) & ~IPAddr.new(netmask)
  index = index.to_i
  stop_address = IPAddr.new(rangeH["end"]) & ~IPAddr.new(netmask)
  stop_address = IPAddr.new(subnet) | (stop_address.to_i + 1)
  address = IPAddr.new(subnet) | index

  network_list = []
  while address != stop_address
    network_list << address.to_s
    index = index + 1
    address = IPAddr.new(subnet) | index
  end
  network_list << address.to_s

  template "/etc/mysql/nova-fixed-range.sql" do
    path "/etc/mysql/nova-fixed-range.sql"
    source "fixed-range.sql.erb"
    owner "root"
    group "root"
    mode "0600"
    variables(
      :network => network_list
    )
    notifies :run, resources(:execute => "mysql-fix-ranges-fixed"), :immediately
  end
end

# User Credentials and Environment Settings
execute "nova-manage project zipfile #{node[:nova][:project]} #{node[:nova][:user]} #{node[:nova][:user_dir]}/nova.zip" do
  user node[:nova][:user]
  not_if { ::File.exists?("#{node[:nova][:user_dir]}/nova.zip")}
end

execute "unzip -o /var/lib/nova/nova.zip -d #{node[:nova][:user_dir]}/" do
  user node[:nova][:user]
  group node[:nova][:group]
  not_if { ::File.exists?("#{node[:nova][:user_dir]}/novarc")}
end

link "#{node[:nova][:user_dir]}/.bashrc" do
  to "#{node[:nova][:user_dir]}/novarc"
  owner node[:nova][:user]
  group node[:nova][:user_group]
end

link "#{node[:nova][:user_dir]}/.profile" do
  to "#{node[:nova][:user_dir]}/novarc"
  owner node[:nova][:user]
  group node[:nova][:user_group]
end

#for the euca2ools
link "#{node[:nova][:user_dir]}/.eucarc" do
  to "#{node[:nova][:user_dir]}/novarc"
  owner node[:nova][:user]
  group node[:nova][:user_group]
end

#generate a private key
execute "euca-add-keypair --config #{node[:nova][:user_dir]}/novarc mykey > #{node[:nova][:user_dir]}/mykey.priv" do
  user node[:nova][:user]
  not_if { ::File.exists?("#{node[:nova][:user_dir]}/mykey.priv")}
end

execute "chmod 0600 #{node[:nova][:user_dir]}/mykey.priv" do
  user node[:nova][:user]
end

file "/var/lib/nova/setup" do
  action :touch
  not_if { ::File.exists?("/var/lib/nova/setup") }
end

cmd = Chef::ShellOut.new("sudo -i -u #{node[:nova][:user]} euca-describe-groups")
groups = cmd.run_command
Chef::Log.debug groups

execute "euca-authorize --config #{node[:nova][:user_dir]}/novarc -P icmp -t -1:-1 default" do
  user node[:nova][:user]
  not_if {groups.stdout.include?("icmp")}
end

execute "euca-authorize --config #{node[:nova][:user_dir]}/novarc -P tcp -p 22 default" do
  user node[:nova][:user]
  not_if {groups.stdout.include?("tcp")}
end

#debug output
# execute "nova-manage service list" do
#   user node[:nova][:user]
# end

#download and install AMIs
(node[:nova][:images] or []).each do |image|
  #get the filename of the image
  filename = image.split('/').last
  bash "upload_image #{filename}" do
    code <<-EOH
BASH_SOURCE=#{node[:nova][:user_dir]}
. ../.eucarc
uec-publish-tarball #{filename} nova_amis amd64 
EOH
    cwd "#{node[:nova][:user_dir]}/images/"
    user node[:nova][:user]
    action :nothing
  end
  remote_file image do
    source image
    path "#{node[:nova][:user_dir]}/images/#{filename}"
    owner node[:nova][:user]
    action :create_if_missing
    notifies :run, "bash[upload_image #{filename}]", :immediately
  end
end

# #debug output
# execute "euca-describe-images" do
#   user node[:nova][:user]
# end

