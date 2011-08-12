# Copyright 2011, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

admin_ip = Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, "admin").address
domain_name = node[:dns].nil? ? node[:domain] : (node[:dns][:domain] || node[:domain])
web_port = node[:provisioner][:web_port]
use_local_security = node[:provisioner][:use_local_security]

image="redhat_install"
rel_path= "redhat_dvd/#{image}"
install_path = "/tftpboot/#{rel_path}"

admin_web="http://#{admin_ip}:#{web_port}/redhat_dvd"
append_line="method=#{admin_web} ks=#{admin_web}/#{image}/compute.ks ksdevice=bootif initrd=../images/pxeboot/initrd.img"

if node[:provisioner][:use_serial_console]
  append_line = "console=tty0 console=ttyS1,115200n8 " + append_line
end
if ::File.exists?("/etc/crowbar.install.key")
  append_line = "crowbar.install.key=#{::File.read("/etc/crowbar.install.key").chomp.strip} " + append_line
end

# Make sure the directories need to net_install are there.
directory "#{install_path}" do  
  recursive true  
end

directory "#{install_path}/pxelinux.cfg"

# Everyone needs a pxelinux.0
bash "Install pxelinux.0" do
  code "cp /usr/lib/syslinux/pxelinux.0 #{install_path}"
  not_if do ::File.exists?("#{install_path}/pxelinux.0") end
end

dhcp_group image do
  action :add
  options [ "option domain-name \"#{domain_name}\"",
              "option dhcp-client-state 2",
              "filename \"#{rel_path}/pxelinux.0\"" ]
end

template "#{install_path}/compute.ks" do
  template ="compute.ks.erb"
  user "apache"
  group "apache"
  variables (
  :admin_node_ip => admin_ip,
  :install_web_port => web_port)  
end

template "#{install_path}/pxelinux.cfg/default" do
  mode 0644
  owner "root"
  group "root"
  source "default.erb"
  variables(:append_line => "append " + append_line,
            :install_name => image,  
            :kernel => "../images/pxeboot/vmlinuz")
end

template "#{install_path}/crowbar_join.sh" do
  mode 0644
  owner "root"
  group "root"
  source "crowbar_join.sh.erb"
  variables(:admin_ip => admin_ip)
end
