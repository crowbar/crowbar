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

class ==^BC-MODEL==Service < ServiceObject

  def create_proposal(name)
    @logger.debug("==*BC-MODEL== create_proposal: entering")
    base = super(name)

    nodes = Node.all
    nodes.delete_if { |n| n.nil? or n.is_admin? }
    if nodes.size >= 1
      add_role_to_instance_and_node(nodes.first.name, base.name, "==BC-MODEL==-server")
    end

    @logger.debug("==*BC-MODEL== create_proposal: exiting")
    base
  end

  def apply_role_pre_chef_call(old_role, new_config, all_nodes)
    @logger.debug("==*BC-MODEL== apply_role_pre_chef_call: entering #{all_nodes.inspect}")
    return if all_nodes.empty?

    # Make sure the bind hosts are in the admin network
    all_nodes.each do |node|
      admin_address = node.address.addr

      hash = new_config.get_node_config_hash(node)
      hash[:==BC-MODEL==] = {} if hash[:==BC-MODEL==].nil?
      hash[:==BC-MODEL==][:api_bind_host] = admin_address
      new_config.set_node_config_hash(node, hash)
    end
    @logger.debug("==*BC-MODEL== apply_role_pre_chef_call: leaving")
  end

end

