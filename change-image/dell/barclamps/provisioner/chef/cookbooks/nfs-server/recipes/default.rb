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

case node[:platform]
when "ubuntu","debian"
  package "nfs-common"
  package "nfs-kernel-server"
when "centos","redhat"
  package "nfs-utils"
end
package "portmap"



service "portmap" do
  running true
  enabled true
  action [ :enable, :start ]
end


link "/updates" do
  to "/tftpboot/#{node[:platform]}_dvd/updates"
end

directory "/install-logs" do
  owner "root"
  group "root"
  mode 0755
end

service "nfs-kernel-server" do
  service_name "nfs" if node[:platform] =~ /^(redhat|centos)$/
  supports :restart => true, :status => true, :reload => true
  running true
  enabled true
  action [ :enable, :start ]
end

template "/etc/exports" do
  source "exports.erb"
  group "root"
  owner "root"
  mode 0644
  variables(:admin_subnet => node["network"]["networks"]["admin"]["subnet"],
            :admin_netmask => node["network"]["networks"]["admin"]["netmask"])
  notifies :run, "execute[nfs-export]", :delayed
end

execute "nfs-export" do
  command "exportfs -a"
  action :run
end

