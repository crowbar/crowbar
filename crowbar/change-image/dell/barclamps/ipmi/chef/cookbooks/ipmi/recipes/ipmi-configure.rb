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



# for now, assume it's there.
# package "ipmitool"


bmc_user     = node[:ipmi][:bmc_user]
bmc_password = node[:ipmi][:bmc_password]
bmc_address  = node[:crowbar][:network][:bmc][:address]
bmc_netmask  = node[:crowbar][:network][:bmc][:netmask]
bmc_router   = node[:crowbar][:network][:bmc][:router]

def log_debug(msg)
  log(msg) {level :info} if node[:ipmi][:debug]
end

## utility to check if a lan parameter needs to be set (if it's current value is different than desired one).
def check_ipmi_lan_value(header, desired)  
  c_awk = "awk -F : ' /#{header}\\W*:/ { print \$2 } '"
  current = %x{ipmitool lan print 1 | #{c_awk} }
  current = current.chomp.strip
  log_debug  " header #{header} should have: #{desired} current: #{current}"
  ret = current.casecmp(desired) == 0 # true if matching
  log_debug ("#{ret ? "will not": "will" } change value" )
  ret
end



if node[:ipmi][:bmc_enable]
  # Make sure the IPMI kernel modules are installed 
  bash "install-ipmi_si" do
    code "/sbin/modprobe ipmi_si"
    not_if { ::File.exists?("/sys/module/ipmi_si") }
    returns [0,1]
    ignore_failure true
  end
  
  bash "install-devintf" do
    code "/sbin/modprobe ipmi_devintf"
    not_if { ::File.exists?("/sys/module/ipmi_devintf") }
    returns [0,1]
    ignore_failure true
  end
  
  ## failed to load ipmi support... occurs on virtual machines and machines without IPMI
  if !File.exists?("/sys/module/ipmi_si")
    node[:ipmi][:bmc_enable] = false  # won't try again.. 
    log ("Unsupported product found #{node[:dmi][:system][:product_name]} - skipping IPMI") {level :warn}
  else
    ### lan parameters to check and set. The loop that follows iterates over this array.
    # [0] = name in "print" output, [1] command to issue, [2] desired value.
    lan_params = [
    [ "IP Address" ,"ipmitool lan set 1 ipaddr #{bmc_address}", bmc_address ] ,
    [ "IP Address Source" ,"ipmitool lan set 1 ipsrc static", "Static Address" ] ,
    [ "Subnet Mask" , "ipmitool lan set 1 netmask #{bmc_netmask}", bmc_netmask ]
    ]
    
    lan_params << [ "Default Gateway IP", "ipmitool lan set 1 defgw ipaddr #{bmc_router}", bmc_router ] unless bmc_router.nil?
    
    
    # Set BMC LAN parameters 
    lan_params.each { |param| 
      bash "bmc-set-lan-#{param[0]}" do
        code <<-EOH
#{param[1]}
sleep 1
EOH
      end unless check_ipmi_lan_value(param[0], param[2])
    } 
    
    # Set the BMC channel user parameters 
    bash "bmc-set-user" do
      code <<-EOH
  ipmitool user set name 3 #{bmc_user}
  sleep 1
  ipmitool user set password 3 #{bmc_password}
  sleep 1
  ipmitool user priv 3 4 1
  sleep 1
  ipmitool channel setaccess 1 3 callin=on link=on ipmi=on privilege=4
  sleep 1
  ipmitool user enable 3
  sleep 1
EOH
    end 
    
    s = "" 
    s << "BMC info:  user: [#{bmc_user}] "
    s << "password [#{bmc_password}] "
    s << "address:[#{bmc_address}] "
    s << "netmask  [#{bmc_netmask}]"
    s << "router   [#{bmc_router}]" unless bmc_router.nil?
    log (s) { level :info } if node[:ipmi][:debug]
    
  end 
end
