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

case node[:platform]
when "ubuntu", "debian"
  package "tftpd-hpa"
  cookbook_file "/etc/default/tftpd-hpa" do
    owner "root"
    group "root"
    mode 0644
    source "tftpd-hpa"
    notifies :restart, "service[tftpd-hpa]"
  end

  service "tftpd-hpa" do
    provider Chef::Provider::Service::Upstart if node[:platform] == "ubuntu"
    supports :restart => true, :status => true, :reload => true
    running true
    enabled true
    action [ :enable, :start ]
  end
when "redhat","centos"
  package "tftp-server"
  package "xinetd"
  bash "enable tftp from xinetd" do
    code "sed -i -e '/disable/ s/yes/no/' /etc/xinetd.d/tftp"
    not_if "chkconfig --list tftp |grep -q on"
  end
  service "xinetd" do
    supports :status => false
    action [ :restart, :enable ]
  end
end

