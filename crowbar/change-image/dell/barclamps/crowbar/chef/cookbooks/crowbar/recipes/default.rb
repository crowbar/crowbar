#
# Cookbook Name:: crowbar
# Recipe:: default
#
# Copyright 2011, Opscode, Inc. and Dell, Inc
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

include_recipe "apache2"
include_recipe "apache2::mod_auth_digest"
include_recipe "passenger_apache2"
include_recipe "rails"

package "ipmitool"
package "curl"

group "crowbar"

user "crowbar" do
  comment "Crowbar"
  gid "crowbar"
  home "/home/random"
  shell "/bin/false"
end

directory "/root/.chef" do
  owner "root"
  group "root"
  mode "0700"
  action :create
end

cookbook_file "/etc/profile.d/crowbar.sh" do
  owner "root"
  group "root"
  mode "0755"
  action :create
  source "crowbar.sh"
end

cookbook_file "/root/.chef/knife.rb" do
  owner "root"
  group "root"
  mode "0600"
  action :create
  source "knife.rb"
end

directory "/home/openstack/.chef" do
  owner "openstack"
  group "openstack"
  mode "0700"
  action :create
end

cookbook_file "/home/openstack/.chef/knife.rb" do
  owner "openstack"
  group "openstack"
  mode "0600"
  action :create
  source "knife.rb"
end

bash "Add crowbar chef client" do
  environment ({'EDITOR' => '/bin/true'})
  code "knife client create crowbar -a --file /opt/dell/openstack_manager/config/client.pem -u chef-validator -k /etc/chef/validation.pem"
  not_if "knife client list -u crowbar -k /opt/dell/openstack_manager/config/client.pem"
end

file "/opt/dell/openstack_manager/log/production.log" do
  owner "crowbar"
  group "crowbar"
  mode "0666"
  action :create
end

file "/opt/dell/openstack_manager/tmp/queue.lock" do
  owner "crowbar"
  group "crowbar"
  mode "0644"
  action :create
end
file "/opt/dell/openstack_manager/tmp/ip.lock" do
  owner "crowbar"
  group "crowbar"
  mode "0644"
  action :create
end

# Add crowbar users - system added machine-install in install-chef.sh
node["crowbar"]["users"].each do |user,hash|
  crowbar_user user do
    action :add
    description hash["description"]
    password hash["password"]
  end
end unless node["crowbar"].nil? or node["crowbar"]["users"].nil?

bash "set permissions" do
  code "chown -R crowbar:crowbar /opt/dell/openstack_manager"
  not_if "ls -al /opt/dell/openstack_manager/README | grep -q crowbar"
end

web_app "crowbar_app" do
  server_name node[:fqdn]
  docroot "/opt/dell/openstack_manager/public"
  template "crowbar_app.conf.erb"
end

cookbook_file "/etc/init.d/crowbar" do
  owner "root"
  group "root"
  mode "0755"
  action :create
  source "crowbar"
end

["3", "5", "2"].each do |i|
  link "/etc/rc#{i}.d/S99xcrowbar" do
    action :create
    to "/etc/init.d/crowbar"
    not_if "test -L /etc/rc#{i}.d/S99xcrowbar"
  end
end

