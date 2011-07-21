#
# Cookbook Name:: glance
# Recipe:: setup
#

include_recipe "#{@cookbook_name}::common"

bash "tty linux setup" do
  cwd "/tmp"
  user "root"
  code <<-EOH
	mkdir -p /var/lib/glance/
	curl #{node[:glance][:tty_linux_image]} | tar xvz -C /tmp/
	glance add name="ari-tty" type="ramdisk" disk_format="ari" container_format="ari" is_public=true < /tmp/tty_linux/ramdisk
	glance add name="aki-tty" type="kernel" disk_format="aki" container_format="aki" is_public=true < /tmp/tty_linux/kernel
	glance add name="ami-tty" type="kernel" disk_format="ami" container_format="ami" ramdisk_id="1" kernel_id="2" is_public=true < /tmp/tty_linux/image
	touch /var/lib/glance/tty_setup
  EOH
  not_if do File.exists?("/var/lib/glance/tty_setup") end
end
