
package "ntp"

if node["roles"].include?("ntp-client")
  unless Chef::Config[:solo]
    env_filter = " AND environment:#{node[:ntp][:config][:environment]}"
    servers = search(:node, "roles:ntp\\-server#{env_filter}")
  end
  ntp_servers = nil
  ntp_servers = servers.map {|n| Ntp::Evaluator.get_value_by_type(n, :admin_ip_eval) } unless servers.nil?
else
  ntp_servers = node[:ntp][:external_servers]
end

service "ntp" do
  supports :restart => true, :status => true, :reload => true
  running true
  enabled true
  action [ :enable, :start ]
end

template "/etc/ntp.conf" do
  owner "root"
  group "root"
  mode 0644
  source "ntp.conf.erb"
  variables(:ntp_servers => ntp_servers)
  notifies :restart, "service[ntp]"
end

#
# Make sure the ntpdate helper is removed to speed up network restarts
# This script manages ntp for the client
#
file "/etc/network/if-up.d/ntpdate" do
  action :delete
end

