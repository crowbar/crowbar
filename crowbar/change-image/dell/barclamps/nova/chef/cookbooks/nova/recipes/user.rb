#
# Cookbook Name:: nova
# Recipe:: user
#
# Copyright 2011, Anso Labs
# Copyright 2011, Dell, Inc.
# Copyright 2011, Opscode, Inc.
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

group node[:nova][:user_group] do
  group_name node[:nova][:user_group]
  action :create
end

user node[:nova][:user] do
  group node[:nova][:user_group]
  comment "Nova User"
  home node[:nova][:user_dir]
  shell "/bin/bash"
  action :create
end

directory node[:nova][:user_dir] do
  owner node[:nova][:user]
  group node[:nova][:user_group]
  mode "0755"
  action :create
end
