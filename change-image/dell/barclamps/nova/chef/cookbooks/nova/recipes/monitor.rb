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
# Author: Greg Althaus
#

####
# if monitored by nagios, install the nrpe commands

# Nova scale data holder
nova_scale = {
  :computes => [],
  :volumes => [],
  :networks => [],
  :schedulers => [],
  :apis => [],
  :objectstores => []
}

unless node[:nova_environment].nil?
  search(:node, "roles:nova-single-machine AND nova_environment:#{node[:nova_environment]}") do |n|
    nova_scale[:computes] << n
#   nova_scale[:volumes] << n
    nova_scale[:networks] << n
    nova_scale[:schedulers] << n
    nova_scale[:apis] << n
    nova_scale[:objectstores] << n
  end

  search(:node, "roles:nova-multi-controller AND nova_environment:#{node[:nova_environment]}") do |n|
    nova_scale[:networks] << n
    nova_scale[:schedulers] << n
    nova_scale[:apis] << n
    nova_scale[:objectstores] << n
  end

  search(:node, "roles:nova-multi-compute AND nova_environment:#{node[:nova_environment]}") do |n|
    nova_scale[:computes] << n
#   nova_scale[:volumes] << n
  end
  # As other nova and swift roles come on-line we should add them here.
end

template "/etc/nagios/nrpe.d/nova_nrpe.cfg" do
  source "nova_nrpe.cfg.erb"
  mode "0644"
  group node[:nagios][:group]
  owner node[:nagios][:user]
  variables( {
    :nova_scale => nova_scale
  })    
   notifies :restart, resources(:service => "nagios-nrpe-server")
end if node["roles"].include?("nagios-client")    

