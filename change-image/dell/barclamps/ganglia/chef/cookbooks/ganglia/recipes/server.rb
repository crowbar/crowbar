# GANLIA monitoring server recipe
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


include_recipe "ganglia::client"

case node[:platform]
when "ubuntu","debian"
  gmetad_pkg_name = "gmetad"
  gmeta_config_file = "/etc/ganglia/gmetad.conf"
  gmetad_svc_name = "gmetad"
  ganglia_web_pkg_name = "ganglia-webfrontend"
  ubuntu_os = true
when "redhat","centos"
  gmetad_pkg_name = "ganglia-gmetad"
  gmetad_config_file = "/etc/ganglia/gmetad.conf"
  gmetad_svc_name = "gmetad"
  ganglia_web_pkg_name = "ganglia-web"
  ubuntu_os = false
end

package gmetad_pkg_name
package ganglia_web_pkg_name

# Begin recipe transactions
Chef::Log.debug("BEGIN ganglia-server")

if ubuntu_os 
  link "/etc/apache2/conf.d/ganglia.conf" do
    to "/etc/ganglia-webfrontend/apache.conf"
    not_if "test -L /etc/apache2/conf.d/ganglia.conf"
    notifies :reload, "service[apache2]"
  end
end

template gmetad_config_file do
  source "gmetad.conf.erb" 
  notifies :restart, "service[gmetad]"
end

service "gmetad" do
  service_name gmetad_svc_name
  supports :restart => true
  running true
  enabled true
  action [ :enable, :start ]
end

# End of recipe transactions
Chef::Log.debug("END ganglia-server")

