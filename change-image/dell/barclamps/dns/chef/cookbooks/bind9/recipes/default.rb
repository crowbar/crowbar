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

include_recipe "utils"

package "bind9"
package "bind9utils"

template "/etc/bind/named.conf.options" do
  source "named.conf.options.erb"
  variables(:forwarders => node[:dns][:forwarders])
  mode 0644
  owner "root"
  group "bind"
  notifies :restart, "service[bind9]"
end

service "bind9" do
  supports :restart => true, :status => true, :reload => true
  running true
  enabled true
  action :enable
end

file "/etc/bind/hosts" do
  owner "root"
  group "root"
  mode 0644
  content ""
  action :create
  not_if do File.exists?("/etc/bind/hosts") end
end

file "/etc/bind/netargs" do
  owner "root"
  group "root"
  mode 0644
  content ""
  action :create
  not_if do File.exists?("/etc/bind/netargs") end
end

bash "build-domain-file" do
  code <<-EOH
    mkdir /tmp/tmp.$$
    cd /tmp/tmp.$$

    NET_ARGS=`cat /etc/bind/netargs | while read line
    do
      echo -n "$line "
    done`

    /opt/dell/bin/h2n -d #{node[:dns][:domain]} -u #{node[:dns][:contact]} $NET_ARGS -H /etc/bind/hosts -h localhost +c named.conf.local -q
    rm -f boot.cacheonly conf.cacheonly db.127.0.0 named.boot dns.hosts
    sed -i 's/"db/"\\/etc\\/bind\\/db/' named.conf.local
    grep zone named.conf.local | grep -v "zone \\".\\"" | grep -v "0.0.127" > named.conf.new
    mv named.conf.new named.conf.local
    cp * /etc/bind

    rm -rf /tmp/tmp.$$
EOH
  action :nothing
  notifies :restart, resources(:service => "bind9"), :immediately
end

#
# This relies on the network barclamp networks - GREG: Not sure I like this!
#
storage_network = data_bag_item('crowbar', 'storage_network')
admin_network = data_bag_item('crowbar', 'admin_network')
bmc_network = data_bag_item('crowbar', 'bmc_network')

[ admin_network, bmc_network, storage_network ].each do |network|
  network_name = network[:id].gsub("_network","")
  base_name = ""
  network[:allocated].each do |ip,h|
    base_name = "#{h[:machine]} " if network_name == "admin"
    hostname_str = "#{base_name}#{network_name}.#{h[:machine]}"
    bind9_host ip do
      hostname hostname_str
      action :add
    end
  end
  bind9_net network[:network][:subnet] do
    netmask network[:network][:netmask]
    action :add
  end
end

node[:dns][:static].each do |name,ip|
  bind9_host ip do
    hostname name
    action :add
  end
end

