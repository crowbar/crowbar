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

package "rsyslog"

env_filter = " AND environment:#{node[:logging][:config][:environment]}"
servers = search(:node, "roles:logging\\-server#{env_filter}")

if servers.nil?
  servers = []
else
  servers = servers.map { |x| Chef::Recipe::Barclamp::Inventory.get_network_by_type(x, "admin").address }
end

# Disable syslogd in favor of rsyslog on redhat.
case node[:platform]
  when "redhat","centos"
  service "syslog" do
    action [ :stop, :disable]
  end
end

service "rsyslog" do
  provider Chef::Provider::Service::Upstart if node[:platform] == "ubuntu"
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

