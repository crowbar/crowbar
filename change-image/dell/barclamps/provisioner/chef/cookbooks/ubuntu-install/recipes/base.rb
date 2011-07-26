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

package "ipmitool"

directory "/root/.ssh" do
  owner "root"
  group "root"
  mode "0700"
  action :create
end

cookbook_file "/root/.ssh/authorized_keys" do
  owner "root"
  group "root"
  mode "0700"
  action :create
  source "authorized_keys"
end

cookbook_file "/etc/default/chef-client" do
  owner "root"
  group "root"
  mode "0644"
  action :create
  source "chef-client"
end

