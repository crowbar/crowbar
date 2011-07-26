#
# Cookbook Name:: glance
# Attributes:: default
#
# Copyright 2011, Opscode, Inc.
# Copyright 2011, Dell, Inc.
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

override[:glance][:user]="glance"

default[:glance][:config_file]="/etc/glance/glance.conf"
default[:glance][:working_directory]="/var/lib/glance"
default[:glance][:pid_directory]="/var/run/glance/"

default[:glance][:verbose] = "True"
default[:glance][:debug] = "True"
default[:glance][:api_bind_host] = ipaddress
default[:glance][:api_bind_port] = "9292"
default[:glance][:registry_bind_host] = ipaddress
default[:glance][:registry_bind_port] = "9191"
default[:glance][:sql_connection] = "sqlite:////var/lib/glance/glance.sqlite"
default[:glance][:sql_idle_timeout] = "3600"

#default_store choices are: file, http, https, swift, s3
default[:glance][:default_store] = "file"
default[:glance][:filesystem_store_datadir] = "/var/lib/glance/images"

default[:glance][:swift_store_auth_address] = "127.0.0.1:8080/v1.0/"
default[:glance][:swift_store_user] = "swiftuser"
default[:glance][:swift_store_key] = "swift_store_key"
default[:glance][:swift_store_container] = "glance"
default[:glance][:swift_store_create_container_on_put] = "False"

# automatically glance upload the tty linux image. (glance::setup recipe)
default[:glance][:tty_linux_image] = "http://c3226372.r72.cf0.rackcdn.com/tty_linux.tar.gz"

# declare what needs to be monitored
node[:glance][:monitor]={}
node[:glance][:monitor][:svcs] = []
node[:glance][:monitor][:ports]={}

