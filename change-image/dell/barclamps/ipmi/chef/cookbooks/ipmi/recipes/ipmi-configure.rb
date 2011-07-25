#
# Copyright (c) 2011 Dell Inc.
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
# Note : This script runs on both the admin and compute nodes.
# It intentionally ignores the bios->enable node data flag.

include_recipe "utils"

platform = node[:platform]
case platform
  when "ubuntu"
    package "ipmitool"
end

bmc_user     = node[:ipmi][:bmc_user]
bmc_password = node[:ipmi][:bmc_password]
bmc_address  = node[:crowbar][:network][:bmc][:address]
bmc_netmask  = node[:crowbar][:network][:bmc][:netmask]
bmc_router   = node[:crowbar][:network][:bmc][:router]

node["crowbar"] = {} if node["crowbar"].nil?
node["crowbar"]["status"] = {} if node["crowbar"]["status"].nil?
node["crowbar"]["status"]["ipmi"] = {} if node["crowbar"]["status"]["ipmi"].nil?
node["crowbar"]["status"]["ipmi"]["user_set"] = false
node["crowbar"]["status"]["ipmi"]["address_set"] = false
node.save

if node[:ipmi][:bmc_enable]
  node["crowbar"]["status"]["ipmi"]["messages"] = []
  node.save

  ipmi_load "ipmi_load" do
    action :run
  end

  ### lan parameters to check and set. The loop that follows iterates over this array.
  # [0] = name in "print" output, [1] command to issue, [2] desired value.
  lan_params = [
    [ "IP Address Source" ,"ipmitool lan set 1 ipsrc static", "Static Address" ] ,
    [ "IP Address" ,"ipmitool lan set 1 ipaddr #{bmc_address}", bmc_address ] ,
    [ "Subnet Mask" , "ipmitool lan set 1 netmask #{bmc_netmask}", bmc_netmask ]
  ]

  lan_params << [ "Default Gateway IP", "ipmitool lan set 1 defgw ipaddr #{bmc_router}", bmc_router ] unless bmc_router.nil?

  lan_params.each do |param| 
    ipmi_lan_set "#{param[0]}" do
      command param[1]
      value param[2]  
      action :run
    end
  end

  ipmi_user_set "#{bmc_user}" do
    password bmc_password
    action :run
  end
end

