#
# Copyright 2011, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Author: andi abes
#

include_recipe 'apt'
include_recipe 'utils'
include_recipe 'swift::auth'

%w{curl  python-software-properties memcached swift-proxy}.each do |pkg|
  package pkg
end

## note that trying to use the <bash> resource fails in odd ways...
execute "create auth cert" do
  cwd "/etc/swift"
  creates "/etc/swift/cert.crt"
  group node[:swift][:group]
  user node[:swift][:user]
  command <<-EOH
  /usr/bin/openssl req -new -x509 -nodes -out cert.crt -keyout cert.key -batch &>/dev/null 0</dev/null
  EOH
  not_if  {::File.exist?("/etc/swift/cert.crt") } 
end

## Find other nodes that are swift-auth nodes, and make sure 
## we use their memcached!
servers =""
env_filter = " AND swift_config_environment:#{node[:swift][:config][:environment]}"
result= search(:node, "(roles:swift-proxy OR roles:swift-proxy-acct) #{env_filter}")
if !result.nil? and (result.length > 0)  
  memcached_servers = result.map {|x|
    s = Swift::Evaluator.get_ip_by_type(x, :admin_ip_expr)     
    s += ":11211 "   
  }
  log("memcached servers" + memcached_servers.join(":")) {level :debug}
  servers = memcached_servers.join(":")
else 
  log("found no swift-proxy nodes") {level :warn}
end

local_ip = Swift::Evaluator.get_ip_by_type(node, :admin_ip_expr)
## Create the proxy server configuraiton file
template "/etc/swift/proxy-server.conf" do
  source "proxy-server.conf.erb"
  mode "0644"
  group node[:swift][:group]
  owner node[:swift][:user]
  variables( {
       :admin_key => node[:swift][:cluster_admin_pw],
       :memcached_ips => servers,
       :localip => local_ip ,
       :user =>node[:swift][:user],
       :debug => node[:swift][:debug],
       :account_management => node[:swift]["account_management"]
  })  
end

## install a default memcached instsance.
## default configuration is take from: node[:memcached] / [:memory], [:port] and [:user] 
node[:memcached][:listen] = local_ip
node[:memcached][:name] = "swift-proxy"
memcached_instance "swift-proxy" do
end


service "swift-proxy" do
  restart_command "/etc/init.d/swift-proxy stop ; /etc/init.d/swift-proxy start"
  action [:enable, :start]
end

bash "restart swift proxy things" do
  code <<-EOH
EOH
  action :run
  notifies :restart, resources(:service => "memcached-swift-proxy")
  notifies :restart, resources(:service => "swift-proxy")
  subscribes :run, resources(:swift_ringfile =>"account.builder")
  subscribes :run, resources(:swift_ringfile =>"container.builder")
  subscribes :run, resources(:swift_ringfile =>"object.builder")
end

### 
# let the monitoring tools know what services should be running on this node.
node[:swift][:monitor] = {}
node[:swift][:monitor][:svcs] = ["swift-proxy", "memcached" ]
node[:swift][:monitor][:ports] = {:proxy =>8080}
node.save
