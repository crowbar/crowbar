# GANLIA monitoring client recipe
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


package "ganglia-monitor"

# Begin recipe transactions
Chef::Log.debug("BEGIN ganlia-client")

admin_interface = Ganglia::Evaluator.get_value_by_type(node,:interface_eval)

template "/etc/ganglia/gmond.conf" do
  source "gmond.conf.erb" 
  variables( :admin_interface => admin_interface )
  notifies :restart, "service[ganglia-monitor]"
end

service "ganglia-monitor" do
  supports :restart => true
  pattern "gmond"
  running true
  enabled true
  action [ :enable, :start ]
end

# End of recipe transactions
Chef::Log.debug("END ganglia-client")

