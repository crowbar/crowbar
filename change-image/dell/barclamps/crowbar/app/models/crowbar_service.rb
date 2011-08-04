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

class CrowbarService < ServiceObject

  def initialize(thelogger)
    @bc_name = "crowbar"
    @logger = thelogger
  end

  #
  # Below are the parts to handle transition requests.
  #
  # This routine handles name-based state transitions.  The system will then inform barclamps.
  # It will create a node and assign it an admin address.
  #
  def transition(inst, name, state)
    save_it = false

    @logger.info("Crowbar transition enter: #{name} to #{state}")

    node = NodeObject.find_node_by_name name
    if node.nil? and (state == "discovering" or state == "testing")
      @logger.debug("Crowbar transition: creating new node for #{name} to #{state}")
      node = NodeObject.create_new name
    end
    if node.nil?
      @logger.error("Crowbar transition leaving: node not found nor created - #{name} to #{state}")
      return [404, "Node not found"]
    end

    node.crowbar["crowbar"] = {} if node.crowbar["crowbar"].nil?
    node.crowbar["crowbar"]["network"] = {} if node.crowbar["crowbar"]["network"].nil?

    pop_it = false
    if (state == "hardware-installing" or state == "hardware-updating" or state == "update") or node.crowbar["state"] != state
      @logger.debug("Crowbar transition: state has changed so we need to do stuff for #{name} to #{state}")

      node.crowbar["crowbar"]["state_debug"] = {} if node.crowbar["crowbar"]["state_debug"].nil?
      if node.crowbar["crowbar"]["state_debug"][state].nil?
        node.crowbar["crowbar"]["state_debug"][state] = 1
      else
        node.crowbar["crowbar"]["state_debug"][state] = node.crowbar["crowbar"]["state_debug"][state] + 1
      end

      node.crowbar["state"] = state
      save_it = true
      pop_it = true
    end

    node.save if save_it

    if pop_it
      crole = RoleObject.find_role_by_name("crowbar-config-#{inst}")
      ro = crole.default_attributes["crowbar"]["run_order"]

      #
      # If we are discovering the node and it is an admin, make sure that we add the crowbar config
      #
      if state == "discovering" and node.admin?
        db = ProposalObject.find_proposal("crowbar", inst)
        add_role_to_instance_and_node("crowbar", inst, name, db, crole, "crowbar")
      end

      roles = RoleObject.find_roles_by_search "transitions:true AND (transition_list:all OR transition_list:#{ChefObject.chef_escape(state)})"
      # Make sure the deployer objects run first.
      roles.sort! do |x,y| 
        xname = x.name.gsub(/-config-.*$/, "")
        yname = y.name.gsub(/-config-.*$/, "")

        xs = 1000
        xs = ro[xname] unless ro[xname].nil?
        ys = 1000
        ys = ro[yname] unless ro[yname].nil?
        xs <=> ys
      end

      roles.each do |role|
        role.override_attributes.each do |bc, data|
          jsondata = {
            "name" => name,
            "state" => state
          }
          rname = role.name.gsub("#{bc}-config-","")
          begin
            @logger.info("Crowbar transition: calling #{bc}:#{rname} for #{name} for #{state}")
            service = eval("#{bc.camelize}Service.new @logger")
            answer = service.transition(rname, name, state)
            if answer[0] != 200
              @logger.error("Crowbar transition: finished #{bc}:#{rname} for #{name} for #{state}: FAILED #{answer[1]}")
            else
              @logger.debug("Crowbar transition: finished #{bc}:#{rname} for #{name} for #{state}")
              unless answer[1]["name"].nil?
                name = answer[1]["name"]
              end
            end
          rescue Exception => e
            @logger.fatal("json/transition for #{bc}:#{rname} failed: #{e.message}")
            @logger.fatal("#{e.backtrace}")
          end
        end
      end

      # We have a node that has become ready, test to see if there are queued proposals to commit
      process_queue if state == "ready"
    end

    @logger.debug("Crowbar transition leaving: #{name} to #{state}")
    [200, NodeObject.find_node_by_name(name).to_hash ]
  end

  def create_proposal
    @logger.debug("Crowbar create_proposal enter")
    base = super
    @logger.debug("Crowbar create_proposal exit")
    base
  end

  def apply_role (role, inst)
    @logger.debug("Crowbar apply_role: enter")
    answer = super
    @logger.debug("Crowbar apply_role: super apply_role finished")

    role = role.default_attributes
    @logger.debug("Crowbar apply_role: create initial instances")
    unless role["crowbar"].nil? or role["crowbar"]["instances"].nil?
      role["crowbar"]["instances"].each do |k,plist|
        plist.each do |v|
          id = "default"
          data = "{\"id\":\"#{id}\"}" 
          if v != "default"
            file = File.open(v, "r")
            data = file.readlines.to_s
            file.close

            struct = JSON.parse(data)
            id = struct["id"].gsub("bc-#{k}-", "")
          end

          @logger.debug("Crowbar apply_role: creating #{k}.#{id}")

          # Create a service to talk to.
          service = eval("#{k.camelize}Service.new @logger")

          @logger.debug("Crowbar apply_role: Calling get to see if it already exists: #{k}.#{id}")
          answer = service.proposals
          if answer[0] != 200
            @logger.error("Failed to list #{k}: #{answer[0]} : #{answer[1]}")
          else
            unless answer[1].include?(id)
              @logger.debug("Crowbar apply_role: didn't already exist, creating proposal for #{k}.#{id}")
              answer = service.proposal_create JSON.parse(data)
              if answer[0] != 200
                @logger.error("Failed to create #{k}.#{id}: #{answer[0]} : #{answer[1]}")
              end
            end
 
            @logger.debug("Crowbar apply_role: check to see if it is already active: #{k}.#{id}")
            answer = service.list_active
            if answer[0] != 200
              @logger.error("Failed to list active #{k}: #{answer[0]} : #{answer[1]}")
            else
              unless answer[1].include?(id)
                @logger.debug("Crowbar apply_role: #{k}.#{id} wasn't active: Activating")
                answer = service.proposal_commit id
                if answer[0] != 200
                  @logger.error("Failed to commit #{k}.#{id}: #{answer[0]} : #{answer[1]}")
                end
              end
            end
          end

          @logger.fatal("Crowbar apply_role: Done with creating: #{k}.#{id}")
        end
      end
    end

    @logger.debug("Crowbar apply_role: leaving: #{answer}")
    answer
  end

end

