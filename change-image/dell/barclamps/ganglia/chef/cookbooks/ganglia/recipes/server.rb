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

package "gmetad"
package "ganglia-webfrontend"

# Begin recipe transactions
Chef::Log.debug("BEGIN ganglia-server")

link "/etc/apache2/conf.d/ganglia.conf" do
  to "/etc/ganglia-webfrontend/apache.conf"
  not_if "test -L /etc/apache2/conf.d/ganglia.conf"
  notifies :reload, "service[apache2]"
end

template "/etc/ganglia/gmetad.conf" do
  source "gmetad.conf.erb" 
  notifies :restart, "service[gmetad]"
end

service "gmetad" do
  supports :restart => true
  running true
  enabled true
  action [ :enable, :start ]
end

# End of recipe transactions
Chef::Log.debug("END ganglia-server")

