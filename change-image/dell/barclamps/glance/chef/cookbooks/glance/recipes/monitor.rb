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

####
# if monitored by nagios, install the nrpe commands

# Node addresses are dynamic and can't be set from attributes only.
node[:glance][:monitor][:ports]["glance-registry"] = [node[:glance][:registry_bind_host], node[:glance][:registry_bind_port]]
node[:glance][:monitor][:ports]["glance-api"] = [node[:glance][:api_bind_host], node[:glance][:api_bind_port]]

svcs = node[:glance][:monitor][:svcs]
ports = node[:glance][:monitor][:ports]
log ("will monitor glance svcs: #{svcs.join(',')} and ports #{ports.values.join(',')}")

template "/etc/nagios/nrpe.d/glance_nrpe.cfg" do
  source "glance_nrpe.cfg.erb"
  mode "0644"
  group node[:nagios][:group]
  owner node[:nagios][:user]
  variables( {
    :svcs => svcs ,
    :ports => ports
  })    
   notifies :restart, resources(:service => "nagios-nrpe-server")
end if node["roles"].include?("nagios-client")    

