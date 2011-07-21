#
# Copyright (c) 2011 Dell Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

default[:ipmi][:config] = {}
default[:ipmi][:config][:environment] = "bios-config-default"

default[:ipmi][:bmc_user] = "crowbar"
default[:ipmi][:bmc_password] = "crowbar"
default[:ipmi][:bmc_enable] = true
default[:ipmi][:debug] = true 

default[:crowbar][:network][:bmc][:address] = "192.168.124.161"
default[:crowbar][:network][:bmc][:netmask] = "255.255.255.0"
default[:crowbar][:network][:bmc][:router] = "192.168.124.10"

