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

## utility to check if a lan parameter needs to be set 
## (if it's current value is different than desired one).
def check_bmc_value(test, desired)
  current = %x{#{test}}
  current = current.chomp.strip
  current.casecmp(desired) == 0
end

action :run do
  name = new_resource.name
  command = new_resource.command
  test = new_resource.test
  value = new_resource.value
  settle_time = new_resource.settle_time

  path = ""
  path = "/tftpboot/ubuntu_dvd" unless ::File.exists?("/updates/bmc")
  path = nil unless ::File.exists?("#{path}/updates/bmc")


  if ::File.exists?("/sys/module/ipmi_si") and !path.nil?
    unless check_bmc_value("#{path}#{test}", value)
      # Set BMC LAN parameters 
      bash "#{name} settle time" do
        code "sleep #{settle_time}"
        action :nothing
      end

      bash "bmc-set-#{name}" do
        code <<-EOH
#{path}#{command}
EOH
        returns [0,5]  # 5 = unsupported platform
        notifies :run, resources(:bash => "#{name} settle time"), :immediately
      end 

      node["crowbar"]["status"]["ipmi"]["messages"] << "#{name} set to #{value}" unless node.nil?
    else
      node["crowbar"]["status"]["ipmi"]["messages"] << "#{name} already set to #{value}" unless node.nil?
    end
  else
    node["ipmi"]["bmc_enable"] = false
    node["crowbar"]["status"]["ipmi"]["messages"] << "Unsupported product found #{node[:dmi][:system][:product_name]} - skipping IPMI:#{name}" unless node.nil?
    node["crowbar"]["status"]["ipmi"]["messages"] << "bmc tool not supported - skipping IPMI:#{name}" unless node.nil? or !path.nil?
  end  
  node.save
end

