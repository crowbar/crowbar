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
  name = new_resource.name
  settle_time = new_resource.settle_time

  unless ::File.exists?("/sys/module/ipmi_devintf") and ::File.exists?("/sys/module/ipmi_si")
    # Make sure the IPMI kernel modules are installed
    bash "install-ipmi_si" do
      code "/sbin/modprobe ipmi_si"
      not_if { ::File.exists?("/sys/module/ipmi_si") }
      returns [0,1]
      ignore_failure true
    end
 
    bash "install-devintf" do
      code "/sbin/modprobe ipmi_devintf"
      not_if { ::File.exists?("/sys/module/ipmi_devintf") }
      returns [0,1]
      ignore_failure true
    end

    bash "settle ipmi load" do
      code "sleep #{settle_time}"
    end
  end
end

