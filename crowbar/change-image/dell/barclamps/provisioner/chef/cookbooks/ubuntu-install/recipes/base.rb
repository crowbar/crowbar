
package "ipmitool"

directory "/root/.ssh" do
  owner "root"
  group "root"
  mode "0700"
  action :create
end

cookbook_file "/root/.ssh/authorized_keys" do
  owner "root"
  group "root"
  mode "0700"
  action :create
  source "authorized_keys"
end

cookbook_file "/etc/default/chef-client" do
  owner "root"
  group "root"
  mode "0644"
  action :create
  source "chef-client"
end

