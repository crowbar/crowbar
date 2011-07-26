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

action :run do
  username = new_resource.name
  password = new_resource.password
  settle_time = new_resource.settle_time

  if ::File.exists?("/sys/module/ipmi_si")
    user_commands = [
      [ "name", "ipmitool user set name 3 #{username}" ],
      [ "password", "ipmitool user set password 3 #{password}" ],
      [ "privs", "ipmitool user priv 3 4 1" ],
      [ "channel", "ipmitool channel setaccess 1 3 callin=on link=on ipmi=on privilege=4" ],
      [ "enable", "ipmitool user enable 3" ]
    ]

    user_commands.each do |param|
      item = param[0]
      command = param[1]

      # Create user stuff
      bash "bmc-set-user-#{item}" do
        code <<-EOH
#{command}
EOH
        ignore_failure true
      end 
      bash "bmc user settle time #{item}" do
        code "sleep #{settle_time}"
      end
      node["crowbar"]["status"]["ipmi"]["messages"] << "Setting user #{item}" unless node.nil?
    end

    node["crowbar"]["status"]["ipmi"]["user_set"] = true
  else
    node["ipmi"]["bmc_enable"] = false
    node["crowbar"]["status"]["ipmi"]["messages"] << "Unsupported product found #{node[:dmi][:system][:product_name]} - skipping IPMI:User" unless node.nil?
  end  
  node.save
end


