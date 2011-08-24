#
# Cookbook Name:: nodejs
# Recipe:: default
#
# Copyright 2011, VMware
#
#
%w[ build-essential ].each do |pkg|
  package pkg
end

remote_file "/tmp/node-v#{node[:nodejs][:version]}.tar.gz" do
  owner node[:deployment][:user]
  source node[:nodejs][:source]
  not_if { ::File.exists?("/tmp/node-v#{node[:nodejs][:version]}.tar.gz") }
end

directory node[:nodejs][:path] do
  owner node[:deployment][:user]
  group node[:deployment][:group]
  mode "0755"
  recursive true
  action :create
end

bash "Install Nodejs" do
 cwd "/tmp"
 user node[:deployment][:user]
 code <<-EOH
 tar xzf node-v#{node[:nodejs][:version]}.tar.gz
 cd node-v#{node[:nodejs][:version]}
 touch step_1
 ./configure --prefix=#{node[:nodejs][:path]}
 touch step_2
 make
 touch step_3
 sudo make install
 rm step_*
 EOH
 not_if do
   ::File.exists?("#{node[:nodejs][:path]}/bin/node")
 end
end
