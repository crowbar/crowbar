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


serial_console = node[:provisioner][:use_serial_console] ? "console=tty0 console=ttyS1,115200n8" : ""
machine_install_key = ::File.read("/etc/crowbar.install.key").chomp.strip
admin_ip = Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, "admin").address
web_port = node[:provisioner][:web_port]
use_local_security = node[:provisioner][:use_local_security]

image="nova_install"
install_path = "/tftpboot/ubuntu_dvd/#{image}"

# Make sure the directories need to net_install are there.
directory "#{install_path}"
directory "#{install_path}/pxelinux.cfg"

# Everyone needs a pxelinux.0
link "#{install_path}/pxelinux.0" do
  action :create
  to "../isolinux/pxelinux.0"
end


append_line="append crowbar.install.key=#{machine_install_key} #{serial_console} url=http://#{admin_ip}:#{web_port}/ubuntu_dvd/#{image}/net_seed debian-installer/locale=en_US.utf8 console-setup/layoutcode=us localechooser/translation/warn-light=true localechooser/translation/warn-severe=true netcfg/choose_interface=auto netcfg/get_hostname=\"redundant\" initrd=../install/netboot/ubuntu-installer/amd64/initrd.gz ramdisk_size=16384 root=/dev/ram rw quiet --"

template "#{install_path}/pxelinux.cfg/default" do
  mode 0644
  owner "root"
  group "root"
  source "default.erb"
  variables(:append_line => append_line,
            :install_name => image,  
            :kernel => "../install/netboot/ubuntu-installer/amd64/linux")
end

template "#{install_path}/net_seed" do
  mode 0644
  owner "root"
  group "root"
  source "net_seed.erb"
  variables(:install_name => image,  
                :cc_use_local_security => use_local_security,
                :cc_install_web_port => web_port,
                :cc_built_admin_node_ip => admin_ip)
end

cookbook_file "#{install_path}/net-post-install.sh" do
  mode 0644
  owner "root"
  group "root"
  source "net-post-install.sh"
end

cookbook_file "#{install_path}/net-pre-install.sh" do
  mode 0644
  owner "root"
  group "root"
  source "net-pre-install.sh"
end

template "#{install_path}/crowbar_join.sh" do
  mode 0644
  owner "root"
  group "root"
  source "crowbar_join.sh.erb"
  variables(:admin_ip => admin_ip)
end

