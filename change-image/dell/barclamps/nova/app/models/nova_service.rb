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

class NovaService < ServiceObject

  def initialize(thelogger)
    @bc_name = "nova"
    @logger = thelogger
  end

  #
  # Lots of enhancements here.  Like:
  #    * Don't reuse machines
  #    * validate hardware.
  #
  def create_proposal
    @logger.debug("Nova create_proposal: entering")
    base = super
    @logger.debug("Nova create_proposal: done with base")

    nodes = NodeObject.all
    nodes.delete_if { |n| n.nil? or n.admin? }
    if nodes.size == 1
      base["deployment"]["nova"]["elements"] = {
        "nova-single-node" => [ nodes.first[:fqdn] ]
      }
    elsif nodes.size > 1
      head = nodes.shift
      base["deployment"]["nova"]["elements"] = {
        "nova-multi-controller" => [ head.name ],
        "nova-multi-compute" => nodes.map { |x| x.name }
      }
    end

    @logger.debug("Nova create_proposal: exiting")
    base
  end

  def apply_role_pre_chef_call(old_role, role, all_nodes)
    @logger.debug("Nova apply_role_pre_chef_call: entering #{all_nodes.inspect}")
    return if all_nodes.empty?

    nodes = NodeObject.find("roles:provisioner-server")
    unless nodes.nil? or nodes.length < 1
      admin_ip = nodes[0].get_network_by_type("admin")["address"]
      web_port = nodes[0]["provisioner"]["web_port"]
      # substitute the admin web portal
      new_array = [] 
      role.default_attributes["nova"]["images"].each do |item|
        new_array << item.gsub("<ADMINWEB>", "#{admin_ip}:#{web_port}")
      end
      role.default_attributes["nova"]["images"] = new_array
      role.save
    end

    # Make sure that the front-end pieces have public ip addreses.
    net_svc = NetworkService.new @logger
    [ "nova-cloud-controller", "nova-multi-controller", "nova-head", "nova-single-machine" ].each do |element|
      tnodes = role.override_attributes["nova"]["elements"][element]
      next if tnodes.nil? or tnodes.empty?
      tnodes.each do |n|
        net_svc.allocate_ip "default", "public", "host", n
        if role.default_attributes["nova"]["network_type"] != "dhcpvlan"
          net_svc.allocate_ip "default", "nova_fixed", "router", n
        end
      end
    end

    all_nodes.each do |n|
      net_svc.enable_interface "default", "nova_fixed", n
    end
    @logger.debug("Nova apply_role_pre_chef_call: leaving")
  end

end

