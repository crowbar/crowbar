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

states = node["provisioner"]["dhcp"]["state_machine"]
nodes = search(:node, "crowbar_usedhcp:true")

if not nodes.nil? and not nodes.empty?
  nodes.each do |mnode|
    next if mnode[:state].nil?

    new_group = nil
    newstate = states[mnode[:state]]
    new_group = newstate if !newstate.nil? && newstate != "noop"

    next if new_group.nil?

    # Delete the node
    system("knife node delete -y #{mnode.name} -u chef-webui -k /etc/chef/webui.pem") if new_group == "delete"
    system("knife node delete -y crowbar-#{mnode.name.gsub(".","_")} -u chef-webui -k /etc/chef/webui.pem") if new_group == "delete"

    admin_data_net = Chef::Recipe::Barclamp::Inventory.get_network_by_type(mnode, "admin")

    # Skip if we don't have admin
    next if admin_data_net.nil?

    mac_list = []
    mnode["network"]["interfaces"].each do |net, net_data|
      net_data.each do |field, field_data|
        next if field != "addresses"
        
        field_data.each do |addr, addr_data|
          next if addr_data["family"] != "lladdr"
          mac_list << addr unless mac_list.include? addr
        end
      end
    end

    # Build entries for each mac address.
    count = 0
    mac_list.each do |mac|
      count = count+1
      if new_group == "reset" or new_group == "delete"
        dhcp_host "#{mnode.name}-#{count}" do
          hostname mnode.name
          ipaddress admin_data_net.address
          macaddress mac
          group new_group
          action :remove
        end
      else
        dhcp_host "#{mnode.name}-#{count}" do
          hostname mnode.name
          ipaddress admin_data_net.address
          macaddress mac
          group new_group
          action :add
        end
      end
    end
  end
end


