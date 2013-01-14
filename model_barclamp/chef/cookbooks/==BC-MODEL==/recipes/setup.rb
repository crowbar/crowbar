#
# Cookbook Name:: ==*BC-MODEL==
# Recipe:: setup
#

include_recipe "#{@cookbook_name}::common"

bash "tty linux setup" do
  cwd "/tmp"
  user "root"
  code <<-EOH
	mkdir -p /var/lib/==BC-MODEL==/
	curl #{node[:==BC-MODEL==][:tty_linux_image]} | tar xvz -C /tmp/
	touch /var/lib/==BC-MODEL==/tty_setup
  EOH
  not_if do File.exists?("/var/lib/==BC-MODEL==/tty_setup") end
end
