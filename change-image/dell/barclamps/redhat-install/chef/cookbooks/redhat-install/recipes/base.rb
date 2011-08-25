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

# Everyone needs chef-client running - redhat doesn't chkconfig this by default.
bash "Install pxelinux.0" do
  code "/sbin/chkconfig --level 345 chef-client on"
  not_if "/sbin/chkconfig --list chef-client | grep -q on"
end

