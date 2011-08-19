#
# Cookbook Name:: essentials
# Recipe:: default
#
# Copyright 2011, VMWARE
#
#

# Add ubuntu apt repositories... some of the packages we need aren't in crowbar (yet)
# Once they are, then we can omit this step.

apt_repository "ubuntu" do
    uri "http://us.archive.ubuntu.com/ubuntu/"
    distribution node[:lsb][:codename]
    components ["main", "restricted", "universe", "multiverse"]
    action :add
end
apt_repository "ubuntu-updates" do
  uri "http://us.archive.ubuntu.com/ubuntu/"
  distribution "#{node[:lsb][:codename]}-updates"
  components ["main", "restricted", "universe", "multiverse"]
  action :add
end
apt_repository "ubuntu-security" do
  uri "http://us.archive.ubuntu.com/ubuntu/"
  distribution "#{node[:lsb][:codename]}-security"
  components ["main", "restricted", "universe", "multiverse"]
  action :add
end


%w{apt-utils wget curl libcurl3 bison build-essential zlib1g-dev libssl-dev
   libreadline5-dev libxml2 libxml2-dev libxslt1.1 libxslt1-dev git-core sqlite3 libsqlite3-ruby
   libsqlite3-dev unzip zip ruby-dev libmysql-ruby libmysqlclient-dev libcurl4-openssl-dev libpq-dev}.each do |p|
  package p do
    action [:install]
  end
end

directory node[:deployment][:config_path] do
  owner node[:deployment][:user]
  group node[:deployment][:group]
  mode "0755"
  recursive true
  action :create
end

directory File.join(node[:deployment][:config_path], "staging") do
  owner node[:deployment][:user]
  group node[:deployment][:group]
  mode "0755"
  recursive true
  action :create
end

%w{/var/vcap /var/vcap/sys /var/vcap/db /var/vcap/sys/log /var/vcap/data/cloud_controller/tmp /var/vcap/data/db /var/vcap.local /var/vcap.local/staging}.each do |d|
  directory d do
    owner node[:deployment][:user]
    group node[:deployment][:group]
    mode "0755"
    recursive true
    action :create
  end
end

template "/etc/sudoers" do
  source "sudoers.erb"
  owner "root"
  group "root"
  mode  "0440"
end
