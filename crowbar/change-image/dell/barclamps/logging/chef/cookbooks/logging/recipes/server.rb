

package "rsyslog"

service "rsyslog" do
  provider Chef::Provider::Service::Upstart
  supports :restart => true, :status => true, :reload => true
  running true
  enabled true
  action [ :enable, :start ]
end

template "/etc/rsyslog.d/10-crowbar-server.conf" do
  owner "root"
  group "root"
  mode 0644
  source "rsyslog.server.erb"
  notifies :restart, "service[rsyslog]"
end

