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

package "ntp" do
  package_name "openntpd" if node[:platform] =~ /^(centos|redhat)$/
  action :install
end

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

user "ntp"
case node[:platform]
when "debian","ubuntu"
  template "/etc/ntp.conf" do
    owner "root"
    group "root"
    mode 0644
    source "ntp.conf.erb"
    variables(:ntp_servers => ntp_servers)
    notifies :restart, "service[ntp]"
  end
when "redhat","centos"
  template "/etc/ntpd.conf" do
    owner "root"
    group "root"
    mode 0644
    source "openntpd.conf.erb"
    variables(:ntp_servers => ntp_servers)
    notifies :restart, "service[ntp]"
  end
end
#
# Make sure the ntpdate helper is removed to speed up network restarts
# This script manages ntp for the client
#
file "/etc/network/if-up.d/ntpdate" do
  action :delete
end if ::File.exists?("/etc/network/if-up.d/ntpdate")

service "ntp" do
  service_name "ntpd" if node[:platform] =~ /^(centos|redhat)$/
  supports :restart => true, :status => true, :reload => true
  running true
  enabled true
  action [ :enable, :start ]
end

