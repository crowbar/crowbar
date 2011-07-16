
package "rsyslog"

env_filter = " AND environment:#{node[:logging][:config][:environment]}"
servers = search(:node, "roles:logging\\-server#{env_filter}")

if servers.nil?
  servers = []
else
  servers = servers.map { |x| Chef::Recipe::Barclamp::Inventory.get_network_by_type(x, "admin").address }
end

service "rsyslog" do
  supports :restart => true, :status => true, :reload => true
  running true
  enabled true
  action [ :enable, :start ]
end

template "/etc/rsyslog.d/10-crowbar-client.conf" do
  owner "root"
  group "root"
  mode 0644
  source "rsyslog.client.erb"
  variables(:servers => servers)
  notifies :restart, "service[rsyslog]"
end

