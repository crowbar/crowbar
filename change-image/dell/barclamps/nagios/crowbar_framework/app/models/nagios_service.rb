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

class NagiosService < ServiceObject

  def initialize(thelogger)
    @bc_name = "nagios"
    @logger = thelogger
  end

  def create_proposal
    @logger.debug("Nagios create_proposal: entering")
    base = super
    @logger.debug("Nagios create_proposal: exiting")
    base
  end

  def transition(inst, name, state)
    @logger.debug("Nagios transition: make sure that network role is on all nodes: #{name} for #{state}")

    #
    # If we are discovering the node, make sure that we add the nagios client or server to the node
    #
    if state == "discovered"
      @logger.debug("Nagios transition: discovered state for #{name} for #{state}")
      db = ProposalObject.find_proposal "nagios", inst
      role = RoleObject.find_role_by_name "nagios-config-#{inst}"

      if role.override_attributes["nagios"]["elements"]["nagios-server"].nil? or
         role.override_attributes["nagios"]["elements"]["nagios-server"].empty?
        @logger.debug("Nagios transition: make sure that nagios-server role is on first: #{name} for #{state}")
        result = add_role_to_instance_and_node("nagios", inst, name, db, role, "nagios-server")
      else
        node = NodeObject.find_node_by_name name
        unless node.role? "nagios-server"
          @logger.debug("Nagios transition: make sure that nagios-client role is on all nodes but first: #{name} for #{state}")
          result = add_role_to_instance_and_node("nagios", inst, name, db, role, "nagios-client")
        else
          result = true
        end
      end

      # Set up the client url
      if result
        role = RoleObject.find_role_by_name "nagios-config-#{inst}"

        # Get the server IP address
        server_ip = nil
        [ "nagios-server" ].each do |element|
          tnodes = role.override_attributes["nagios"]["elements"][element]
          next if tnodes.nil? or tnodes.empty?
          tnodes.each do |n|
            next if n.nil?
            node = NodeObject.find_node_by_name(n)
            server_ip = node.get_network_by_type("admin")["address"]
          end
        end

        unless server_ip.nil?
          node = NodeObject.find_node_by_name(name)
          node.crowbar["crowbar"] = {} if node.crowbar["crowbar"].nil?
          node.crowbar["crowbar"]["links"] = {} if node.crowbar["crowbar"]["links"].nil?
          node.crowbar["crowbar"]["links"]["Nagios"] = "http://#{server_ip}/nagios3/cgi-bin/extinfo.cgi?type=1&host=#{node.shortname}"
          node.save
        end 
      end

      @logger.debug("Nagios transition: leaving from discovered state for #{name} for #{state}")
      a = [200, NodeObject.find_node_by_name(name).to_hash ] if result
      a = [400, "Failed to add role to node"] unless result
      return a
    end

    @logger.debug("Nagios transition: leaving for #{name} for #{state}")
    [200, NodeObject.find_node_by_name(name).to_hash ]
  end

end


