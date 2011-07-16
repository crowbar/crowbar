#
# Cookbook Name:: glance
# Recipe:: common
#
# Copyright 2011 Opscode, Inc.
# Copyright 2011 Rackspace, Inc.
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

#apt_repository "NovaCoreReleasePPA" do
#  uri "http://ppa.launchpad.net/nova-core/release/ubuntu"
#  distribution node["lsb"]["codename"]
#  components ["main"]
#  action :add
#end

package "glance" do
  options "--force-yes"
  action :install
end

template node[:glance][:config_file] do
  source "glance.conf.erb"
  owner node[:glance][:user]
  group "root"
  mode 0644
end

# (node[:nova][:images] or []).each do |image|
#   #get the filename of the image
#   filename = image.split('/').last
#   execute "uec-publish-tarball #{filename} nova_amis x86_64" do
#     cwd "#{node[:nova][:user_dir]}/images/"
#     #need EC2_URL, EC2_ACCESS_KEY, EC2_SECRET_KEY, EC2_CERT, EC2_PRIVATE_KEY, S3_URL, EUCALYPTUS_CERT for environment
#     environment ({
#                    'EC2_URL' => "http://#{node[:nova][:api]}:8773/services/Cloud",
#                    'EC2_ACCESS_KEY' => node[:nova][:access_key],
#                    'EC2_SECRET_KEY' => node[:nova][:secret_key],
#                    'EC2_CERT_' => "#{node[:nova][:user_dir]}/cert.pem",
#                    'EC2_PRIVATE_KEY_' => "#{node[:nova][:user_dir]}/pk.pem",
#                    'S3_URL' => "http://#{node[:nova][:api]}:3333", #TODO need to put S3 into attributes instead of assuming API
#                    'EUCALYPTUS_CERT' => "#{node[:nova][:user_dir]}/cacert.pem"
#                  })
#     user node[:nova][:user]
#     action :nothing
#   end
#   remote_file image do
#     source image
#     path "#{node[:nova][:user_dir]}/images/#{filename}"
#     owner node[:nova][:user]
#     action :create_if_missing
#     notifies :run, resources(:execute => "uec-publish-tarball #{filename} nova_amis x86_64"), :immediately
#   end
# end


# bash "tty linux setup" do
#   cwd "/tmp"
#   user "root"
#   code <<-EOH
# 	mkdir -p /var/lib/glance/
# 	curl #{node[:glance][:tty_linux_image]} | tar xvz -C /tmp/
# 	glance add name="ari-tty" type="ramdisk" disk_format="ari" container_format="ari" is_public=true < /tmp/tty_linux/ramdisk
# 	glance add name="aki-tty" type="kernel" disk_format="aki" container_format="aki" is_public=true < /tmp/tty_linux/kernel
# 	glance add name="ami-tty" type="kernel" disk_format="ami" container_format="ami" ramdisk_id="1" kernel_id="2" is_public=true < /tmp/tty_linux/image
# 	touch /var/lib/glance/tty_setup
#   EOH
#   not_if do File.exists?("/var/lib/glance/tty_setup") end
# end


#download and install AMIs
# (node[:nova][:images] or []).each do |image|
#   #get the filename of the image
#   filename = image.split('/').last
#   execute "uec-publish-tarball #{filename} nova_amis x86_64" do
#     cwd "#{node[:nova][:user_dir]}/images/"
#     #need EC2_URL, EC2_ACCESS_KEY, EC2_SECRET_KEY, EC2_CERT, EC2_PRIVATE_KEY, S3_URL, EUCALYPTUS_CERT for environment
#     environment ({
#                    'EC2_URL' => "http://#{node[:nova][:api]}:8773/services/Cloud",
#                    'EC2_ACCESS_KEY' => node[:nova][:access_key],
#                    'EC2_SECRET_KEY' => node[:nova][:secret_key],
#                    'EC2_CERT_' => "#{node[:nova][:user_dir]}/cert.pem",
#                    'EC2_PRIVATE_KEY_' => "#{node[:nova][:user_dir]}/pk.pem",
#                    'S3_URL' => "http://#{node[:nova][:api]}:3333", #TODO need to put S3 into attributes instead of assuming API
#                    'EUCALYPTUS_CERT' => "#{node[:nova][:user_dir]}/cacert.pem"
#                  })
#     user node[:nova][:user]
#     action :nothing
#   end
#   remote_file image do
#     source image
#     path "#{node[:nova][:user_dir]}/images/#{filename}"
#     owner node[:nova][:user]
#     action :create_if_missing
#     notifies :run, resources(:execute => "uec-publish-tarball #{filename} nova_amis x86_64"), :immediately
#   end
# end

# #debug output
# execute "euca-describe-images" do
#   user node[:nova][:user]
# end

#if not glance

# in /images/

# mkdir kernel image "mykernel_image"
# json file from one with "aki"
# make up id as "mykernel_id"

# mkdir ami image "myami_image"
# json file from one with "ami"
# kernel_id "mykernel_id"
# id "random"

# execute "nova-manage image convert /var/lib/nova/images" do
#   user 'nova'
# end
