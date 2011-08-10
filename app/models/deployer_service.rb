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

class DeployerService < ServiceObject

  def initialize(thelogger)
    @bc_name = "deployer"
    @logger = thelogger
  end

  def create_proposal
    @logger.debug("Deployer create_proposal: entering")
    base = super
    @logger.debug("Deployer create_proposal: leaving")
    base
  end

  def transition(inst, name, state)
    @logger.debug("Deployer transition: entering #{name} for #{state}")

    node = NodeObject.find_node_by_name(name)
    if node.nil?
      @logger.error("Deployer transition: leaving #{name} for #{state}: Node not found")
      return [404, "Failed to find node"]
    end

    # 
    # If we are discovering the node, make sure that we add the deployer client to the node
    #
    if state == "discovering"
      @logger.debug("Deployer transition: leaving #{name} for #{state}: discovering mode")

      db = ProposalObject.find_proposal("deployer", inst)
      role = RoleObject.find_role_by_name "deployer-config-#{inst}"

      result = add_role_to_instance_and_node("deployer", inst, name, db, role, "deployer-client")

      @logger.debug("Deployer transition: leaving #{name} for #{state}: discovering mode: #{result}")
      return [200, NodeObject.find_node_by_name(name).to_hash ] if result
      return [404, "Failed to add role to node"] unless result
    end

    #
    # The temp booting images need to have clients cleared.
    #
    if ["delete","discovered","hardware-installed","hardware-updated",
        "hardware-installing","hardware-updating","reset","reinstall",
        "update"].member?(state) and !node.admin?
      @logger.debug("Deployer transition: should be deleting a client entry for #{node.name}")
      client = ClientObject.find_client_by_name node.name
      @logger.debug("Deployer transition: found and trying to delete a client entry for #{node.name}") unless client.nil?
      client.destroy unless client.nil?

      # Make sure that the node can be accessed by knife ssh or ssh
      if ["reset","reinstall","update","delete"].member?(state)
        system("sudo rm /root/.ssh/known_hosts")
      end
    end

    # if delete - clear out stuff
    if state == "delete"
      # Do more work here - one day.
      return [200, node.to_hash ]
    end

    # 
    # Decide on the nodes role for the cloud
    #   * This includes adding a role for node type (for bios/raid update/config)
    #   * This includes adding an attribute on the node for inclusion in clouds
    # 
    if state == "discovered"
      @logger.debug("Deployer transition: discovered state for #{name}")
      save_it = false

      if !node.admin?
        @logger.debug("Deployer transition: check to see if we should rename: #{name}")
        tname = node.name.split(".")[0]
        tname = tname.gsub!("h", "d")
        new_name = "#{tname}.#{ChefObject.cloud_domain}"
        if new_name != node.name
          @logger.debug("Deployer transition: renaming node for #{name} #{node.name} -> #{new_name}")
          node.destroy

          # Rename saves the node.
          node.rename(new_name, ChefObject.cloud_domain)
          name = new_name
        end
      else # We are an admin node - display bios updates for now.
        node.crowbar["bios"] ||= {}
        node.crowbar["bios"]["bios_setup_enable"] = false
        node.crowbar["bios"]["bios_update_enable"] = false
        node.crowbar["raid"] ||= {}
        node.crowbar["raid"]["enable"] = false
        save_it = true
      end

      #
      # At this point, we need to create our resource maps and recommendations.
      #
      # This is hard coded for now.  Should be parameter driven one day.
      # 
      @logger.debug("Deployer transition: Update the inventory crowbar structures for #{name}")
      node.crowbar["crowbar"] = {} if node.crowbar["crowbar"].nil?
      node.crowbar["crowbar"]["disks"] = {} if node.crowbar["crowbar"]["disks"].nil?
      node[:block_device].each do |disk, data|
        # XXX: Make this into a config map one day.
        next if disk.start_with?("ram")
        next if disk.start_with?("sr")
        next if disk.start_with?("loop")
        next if disk.start_with?("dm")
        next if disk.start_with?("ndb")
        next if disk.start_with?("md")
        next if disk.start_with?("sg")
        next if disk.start_with?("fd")

        next if data[:removable] == 1 or data[:removable] == "1" # Skip cdroms

        # RedHat under KVM reports drives as hdX.  Ubuntu reports them as sdX.
        disk = disk.gsub("hd", "sd") if disk.start_with?("h") and node[:dmi][:system][:product_name] == "KVM"

        node.crowbar["crowbar"]["disks"][disk] = data

        node.crowbar["crowbar"]["disks"][disk]["usage"] = "OS" if disk == "sda"
        node.crowbar["crowbar"]["disks"][disk]["usage"] = "Storage" unless disk == "sda"

        save_it = true
      end unless node[:block_device].nil? or node[:block_device].empty?

      node.crowbar["crowbar"]["usage"] = [] if node.crowbar["crowbar"]["usage"].nil?
      if (node.crowbar["crowbar"]["disks"].size > 1) and !node.crowbar["crowbar"]["usage"].include?("swift")
        node.crowbar["crowbar"]["usage"] << "swift"
        save_it = true
      end

      if !node.crowbar["crowbar"]["usage"].include?("nova")
        node.crowbar["crowbar"]["usage"] << "nova"
        save_it = true
      end

      node.save if save_it

      # Allocate required addresses
      range = node.admin? ? "admin" : "host"
      @logger.debug("Deployer transition: Allocate admin address for #{name}")
      ns = NetworkService.new @logger
      result = ns.allocate_ip("default", "admin", range, name)
      @logger.error("Failed to allocate admin address for: #{node.name}: #{result[0]}") if result[0] != 200
      @logger.debug("Deployer transition: Done Allocate admin address for #{name}")

      @logger.debug("Deployer transition: Allocate bmc address for #{name}")
      result = ns.allocate_ip("default", "bmc", "host", name)
      @logger.error("Failed to allocate bmc address for: #{node.name}: #{result[0]}") if result[0] != 200
      @logger.debug("Deployer transition: Done Allocate bmc address for #{name}")

      # If we are the admin node, we may need to add a vlan bmc address.
      if node.admin?
        # Add the vlan bmc if the bmc network and the admin network are not the same.
        # not great to do it this way, but hey.
        admin_net = ProposalObject.find_data_bag_item "crowbar/admin_network"
        bmc_net = ProposalObject.find_data_bag_item "crowbar/bmc_network"
        if admin_net["network"]["subnet"] != bmc_net["network"]["subnet"]
          @logger.debug("Deployer transition: Allocate bmc_vlan address for #{name}")
          result = ns.allocate_ip("default", "bmc_vlan", "host", name)
          @logger.error("Failed to allocate bmc_vlan address for: #{node.name}: #{result[0]}") if result[0] != 200
          @logger.debug("Deployer transition: Done Allocate bmc_vlan address for #{name}")
        end
      end

      # Let it fly to the provisioner. Reload to get the address.
      node = NodeObject.find_node_by_name node.name
      node.crowbar["crowbar"]["usedhcp"] = true

      role = RoleObject.find_role_by_name "deployer-config-#{inst}"
      if role.default_attributes["deployer"]["use_allocate"] and !node.admin?
        node.allocated = false 
      else
        node.allocated = true
      end

      node.save

      @logger.debug("Deployer transition: leaving discovered for #{name} EOF")
      return [200, node.to_hash ]
    end

    save_it = false
    #
    # Once we have been allocated, we will fly through here and we will setup the raid/bios info
    #
    if state == "hardware-installing"
      # build a list of current and pending roles to check against
      roles = []
      node.crowbar["crowbar"]["pending"].each do |k,v|
        roles << v
      end unless node.crowbar["crowbar"]["pending"].nil?
      roles.flatten!
      node.crowbar_run_list.run_list_items.each do |item|
        roles << item.name
      end

      # Walk map to categorize the node.  Choose first one from the bios map that matches.
      role = RoleObject.find_role_by_name "deployer-config-#{inst}"
      done = false
      role.default_attributes["deployer"]["bios_map"].each do |match|
        roles.each do |r|
          if r =~ /#{match["pattern"]}/
            node.crowbar["crowbar"]["hardware"] = {} if node.crowbar["crowbar"]["hardware"].nil? 
            node.crowbar["crowbar"]["hardware"]["bios_set"] = match["bios_set"] if node.crowbar["crowbar"]["hardware"]["bios_set"].nil?
            node.crowbar["crowbar"]["hardware"]["raid_set"] = match["raid_set"] if node.crowbar["crowbar"]["hardware"]["raid_set"].nil?
            done = true
            break
          end
        end 
        break if done
      end
      
      os_map = role.default_attributes["deployer"]["os_map"]
      node.crowbar["crowbar"]["hardware"]["os"] = os_map[0]["install_os"] 
      save_it = true
    end

    #
    # The node is about to go into update.
    # We should make sure that we save and setup the run-list for updating.
    #
    if state == "update" or state == "hardware-installing"
      save_list = node.crowbar["crowbar"]["save_run_list"] || []
      seen_bios = false
      node.crowbar_run_list.run_list_items.each do |item|
        if seen_bios and !(item.name =~ /^ipmi-/)
          save_list << item.name
        else
          seen_bios = true if item.name =~ /^ipmi-/
        end
      end

      save_list.each do |item|
        node.crowbar_run_list.run_list_items.delete "role[#{item}]"
      end

      node.crowbar["crowbar"]["save_run_list"] = save_list
      save_it = true
    end

    #
    # Put the run-list back.
    #
    if state == "hardware-updated" or state == "hardware-installed"
      unless node.crowbar["crowbar"]["save_run_list"].nil?
        node.crowbar["crowbar"]["save_run_list"].each do |name|
          node.crowbar_run_list.run_list_items << "role[#{name}]"
        end
      end

      node.crowbar["crowbar"]["save_run_list"] = []
      save_it = true
    end

    node.save if save_it

    @logger.debug("Deployer transition: leaving state for #{name} EOF")
    return [200, node.to_hash ]
  end

end

