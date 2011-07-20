
package "tftpd-hpa"

cookbook_file "/etc/default/tftpd-hpa" do
  owner "root"
  group "root"
  mode 0644
  source "tftpd-hpa"
  notifies :restart, "service[tftpd-hpa]"
end

service "tftpd-hpa" do
  provider Chef::Provider::Service::Upstart
  supports :restart => true, :status => true, :reload => true
  running true
  enabled true
  action [ :enable, :start ]
end

