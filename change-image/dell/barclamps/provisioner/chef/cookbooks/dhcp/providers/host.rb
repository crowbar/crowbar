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

action :add do
  Chef::Log.debug "Adding #{new_resource.name}.conf to /etc/dhcp3/hosts.d"
  filename = "/etc/dhcp3/hosts.d/#{new_resource.name}.conf"
  template filename do 
    cookbook "dhcp"
    source "host.conf.erb"
    variables(
      :name => new_resource.name,
      :hostname => new_resource.hostname,
      :macaddress => new_resource.macaddress,
      :ipaddress => new_resource.ipaddress,
      :group => new_resource.group,
      :options => new_resource.options
    )
    owner "root"
    group "root"
    mode 0644
    notifies :restart, resources(:service => "dhcp3-server"), :delayed
  end
  utils_line "include \"#{filename}\";" do
    action :add
    file "/etc/dhcp3/hosts.d/host_list.conf"
    notifies :restart, resources(:service => "dhcp3-server"), :delayed
  end
  if ::File.exists?(filename)
    Chef::Log.debug "#{filename} created."
  else
    Chef::Log.error "#{filename} not created, #{new_resource.name} will loop in hardware-installing forever."
  end
end

action :remove do
  filename = "/etc/dhcp3/hosts.d/#{new_resource.name}.conf"
  if ::File.exists?(filename)
    Chef::Log.info "Removing #{new_resource.name} host from /etc/dhcp3/hosts.d/"
    file filename do
      action :delete
      notifies :restart, resources(:service => "dhcp3-server"), :delayed
    end
    new_resource.updated_by_last_action(true)
  end
  utils_line "include \"#{filename}\";" do
    action :remove
    file "/etc/dhcp3/hosts.d/host_list.conf"
    notifies :restart, resources(:service => "dhcp3-server"), :delayed
  end
end

