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
  utils_line "-n #{new_resource.subnet}:#{new_resource.netmask}" do
    action :add
    file "/etc/bind/netargs"
    notifies :run, resources(:bash => "build-domain-file"), :delayed
  end
end

action :remove do
  utils_line "-n #{new_resource.subnet}:#{new_resource.netmask}" do
    action :remove
    file "/etc/bind/netargs"
    notifies :run, resources(:bash => "build-domain-file"), :delayed
  end
end

