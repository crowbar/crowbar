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

web_port = node[:provisioner][:web_port]
use_local_security = node[:provisioner][:use_local_security]
admin_ip = Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, "admin").address
admin_net = node[:network][:networks]["admin"]

lease_time = node[:provisioner][:dhcp]["lease-time"]
domain_name = node[:dns].nil? ? node[:domain] : (node[:dns][:domain] || node[:domain])
dhcp_start = admin_net[:ranges]["dhcp"]["start"]
dhcp_end = admin_net[:ranges]["dhcp"]["end"]
machine_install_key = ::File.read("/etc/crowbar.install.key").chomp.strip

serial_console = node[:provisioner][:use_serial_console] ? "console=tty0 console=ttyS1,115200n8" : ""

#
# Setup links from centos image to our other two names
#
["update", "hwinstall"].each do |dir|
  link "/tftpboot/ubuntu_dvd/#{dir}" do
    action :create
    to "discovery"
    not_if "test -L /tftpboot/ubuntu_dvd/#{dir}"
  end
end

#
# Update the kernel lines and default configs for our
# three boot environments (centos images are updated
# with discovery).
#
["nova_install", "execute", "discovery"].each do |image|
  install_path = "/tftpboot/ubuntu_dvd/#{image}"

  # Make sure the directories need to net_install are there.
  directory "#{install_path}"
  directory "#{install_path}/pxelinux.cfg"

  # Everyone needs a pxelinux.0
  link "#{install_path}/pxelinux.0" do
    action :create
    to "../isolinux/pxelinux.0"
    not_if "test -L #{install_path}/pxelinux.0"
  end

  case 
  when image == "discovery"
    template "#{install_path}/pxelinux.cfg/default" do
      mode 0644
      owner "root"
      group "root"
      source "default.erb"
      variables(:append_line => "append initrd=initrd0.img #{serial_console} root=/sledgehammer.iso rootfstype=iso9660 rootflags=loop crowbar.install.key=#{machine_install_key}",
                :install_name => image,  
                :kernel => "vmlinuz0")
    end
    next
    
  when image == "execute"
    cookbook_file "#{install_path}/pxelinux.cfg/default" do
      mode 0644
      owner "root"
      group "root"
      source "localboot.default"
    end
    next
  # The nova_install case
  else
    append_line="append crowbar.install.key=#{machine_install_key} #{serial_console} url=http://#{admin_ip}:#{web_port}/ubuntu_dvd/#{image}/net_seed debian-installer/locale=en_US.utf8 console-setup/layoutcode=us localechooser/translation/warn-light=true localechooser/translation/warn-severe=true netcfg/dhcp_timeout=120 netcfg/choose_interface=auto netcfg/get_hostname=\"redundant\" initrd=../install/netboot/ubuntu-installer/amd64/initrd.gz ramdisk_size=16384 root=/dev/ram rw quiet --"
    
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
      source "#{image}.sh"
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
  end
    
end

web_app "install_app" do
  server_name node[:fqdn]
  docroot "/tftpboot"
  template "install_app.conf.erb"
end

bash "copy validation pem" do
  code "cp /etc/chef/validation.pem /tftpboot/ubuntu_dvd"
  not_if "test -f /tftpboot/ubuntu_dvd/validation.pem"  
end

directory "/tftpboot/curl"

[ "/usr/bin/curl",
  "/usr/lib/libcurl.so.4",
  "/usr/lib/libidn.so.11",
  "/usr/lib/liblber-2.4.so.2",
  "/usr/lib/libldap_r-2.4.so.2",
  "/usr/lib/libgssapi_krb5.so.2",
  "/usr/lib/libssl.so.0.9.8",
  "/usr/lib/libcrypto.so.0.9.8",
  "/usr/lib/libsasl2.so.2",
  "/usr/lib/libgnutls.so.26",
  "/usr/lib/libkrb5.so.3",
  "/usr/lib/libk5crypto.so.3",
  "/usr/lib/libkrb5support.so.0",
  "/lib/libkeyutils.so.1",
  "/usr/lib/libtasn1.so.3",
  "/lib/librt.so.1",
  "/lib/libcom_err.so.2",
  "/lib/libgcrypt.so.11",
  "/lib/libgpg-error.so.0"
].each do |file|
  basefile = file.gsub("/usr/bin/", "").gsub("/usr/lib/", "").gsub("/lib/", "")
  bash "copy #{file} to curl dir" do
    code "cp #{file} /tftpboot/curl"
    not_if "test -f /tftpboot/curl/#{basefile}"
  end
end

file "/tftpboot/ubuntu_dvd/validation.pem" do
  mode 0444
end

dhcp_groups = {"nova_install" => 2, "hwinstall" => 1, "update" => 3, "execute" => 4}
dhcp_groups.each do |group, dhcp_state|
  dhcp_group group do
    action :add
    options [ "option domain-name \"#{domain_name}\"",
              "option dhcp-client-state #{dhcp_state}",
              "filename \"/ubuntu_dvd/#{group}/pxelinux.0\"" ]
  end
end

dhcp_subnet admin_net["subnet"] do
  action :add
  broadcast admin_net["broadcast"]
  netmask admin_net["netmask"]
  routers (admin_net["router"].nil? ? [] : [ admin_net["router"] ])
  options [ "option domain-name \"#{domain_name}\"",
            "option domain-name-servers #{admin_ip}",
            "range #{dhcp_start} #{dhcp_end}",
            "default-lease-time #{lease_time}",
            "max-lease-time #{lease_time}",
            'filename "/ubuntu_dvd/discovery/pxelinux.0"' ]
end

states = node["provisioner"]["dhcp"]["state_machine"]
nodes = search(:node, "crowbar_usedhcp:true")
if not nodes.nil? and not nodes.empty?
  nodes.each do |mnode|
    next if mnode[:state].nil?

    new_group = nil
    newstate = states[mnode[:state]]
    new_group = newstate if !newstate.nil? && newstate != "noop"

    next if new_group.nil?

    # Delete the node
    system("knife node delete -y #{mnode.name} -u chef-webui -k /etc/chef/webui.pem") if new_group == "delete"
    system("knife node delete -y crowbar-#{mnode.name.gsub(".","_")} -u chef-webui -k /etc/chef/webui.pem") if new_group == "delete"

    admin_data_net = Chef::Recipe::Barclamp::Inventory.get_network_by_type(mnode, "admin")

    # Skip if we don't have admin
    next if admin_data_net.nil?

    mac_list = []
    mnode["network"]["interfaces"].each do |net, net_data|
      net_data.each do |field, field_data|
        next if field != "addresses"
        
        field_data.each do |addr, addr_data|
          next if addr_data["family"] != "lladdr"
          mac_list << addr unless mac_list.include? addr
        end
      end
    end

    # Build entries for each mac address.
    count = 0
    mac_list.each do |mac|
      count = count+1
      if new_group == "reset" or new_group == "delete"
        dhcp_host "#{mnode.name}-#{count}" do
          hostname mnode.name
          ipaddress admin_data_net.address
          macaddress mac
          group new_group
          action :remove
        end
      else
        dhcp_host "#{mnode.name}-#{count}" do
          hostname mnode.name
          ipaddress admin_data_net.address
          macaddress mac
          group new_group
          action :add
        end
      end
    end
  end
end


