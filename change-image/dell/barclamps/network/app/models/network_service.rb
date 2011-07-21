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

class NetworkService < ServiceObject

  def initialize(thelogger)
    @bc_name = "network"
    @logger = thelogger
  end
  
  def acquire_ip_lock
    acquire_lock "ip"
  end

  def release_ip_lock(f)
    release_lock f
  end

  def allocate_ip(bc_instance, network, range, name)
    @logger.debug("Network allocate_ip: entering #{name} #{network} #{range}")

    return [404, "No network specified"] if network.nil?
    return [404, "No range specified"] if range.nil?
    return [404, "No name specified"] if name.nil?

    # Find the node
    node = NodeObject.find_node_by_name name
    @logger.error("Network allocate_ip: return node not found: #{name} #{network} #{range}") if node.nil?
    return [404, "No node found"] if node.nil?

    # Find an interface based upon config
    role = RoleObject.find_role_by_name "network-config-#{bc_instance}"
    @logger.error("Network allocate_ip: No network data found: #{name} #{network} #{range}") if role.nil?
    return [404, "No network data found"] if role.nil?

    mode = role.default_attributes["network"]["mode"]

    # If we already have on allocated, return success
    net_info = node.get_network_by_type(network)
    unless net_info.nil? or net_info["address"].nil?
      intf = net_info["interface"]
      net_info = fix_interface(node, net_info, mode)
      unless net_info.nil?
        node.crowbar["crowbar"]["network"][intf] = nil
        node.crowbar["crowbar"]["network"][net_info["interface"]] = net_info
        node.save
      end
      @logger.error("Network allocate_ip: node already has address: #{name} #{network} #{range}")
      return [200, net_info]
    end

    found = false
    begin # Rescue block
      f = acquire_ip_lock
      db = ProposalObject.find_data_bag_item "crowbar/#{network}_network"

      subnet = db["network"]["subnet"]
      vlan = db["network"]["vlan"]
      use_vlan = db["network"]["use_vlan"]
      add_bridge = db["network"]["add_bridge"]
      broadcast = db["network"]["broadcast"]
      router = db["network"]["router"]
      netmask = db["network"]["netmask"]
      rangeH = db["network"]["ranges"][range]
      rangeH = db["network"]["ranges"]["host"] if rangeH.nil?

      tanswer = get_interface_info node, network, mode, use_vlan, vlan
      if !tanswer
        @logger.info("Network allocate_ip: return failed to find interface: #{name} #{network} #{range}")
        return [404, "Failed to find interface"]
      end
      interface, interface_list, mac = tanswer

      index = IPAddr.new(rangeH["start"]) & ~IPAddr.new(netmask)
      index = index.to_i
      stop_address = IPAddr.new(rangeH["end"]) & ~IPAddr.new(netmask)
      stop_address = IPAddr.new(subnet) | (stop_address.to_i + 1)
      address = IPAddr.new(subnet) | index

      # Did we already allocate this, but the node lose it?
      unless db["allocated_by_name"][node.name].nil?
	found = true
        address = db["allocated_by_name"][node.name]["address"]
      end

      # Let's search for an empty one.
      while !found do
        if db["allocated"][address.to_s].nil?
          found = true
          break
        end
        index = index + 1
        address = IPAddr.new(subnet) | index
        break if address == stop_address
      end

      if found
        db["allocated_by_name"][node.name] = { "machine" => node.name, "interface" => interface, "address" => address.to_s }
        db["allocated"][address.to_s] = { "machine" => node.name, "interface" => interface, "address" => address.to_s }
        db.save
      end
    rescue Exception => e
      @logger.error("Error finding address: #{e.message}")
    ensure
      release_ip_lock(f)
    end

    @logger.info("Network allocate_ip: no address available: #{name} #{network} #{range}") if !found
    return [404, "No Address Available"] if !found

    # Save the information.
    net_info = { "interface" => interface, "address" => address.to_s, "netmask" => netmask, "mac" => mac, "node" => name, "router" => router, "subnet" => subnet, "broadcast" => broadcast, "usage" => network, "interface_list" => interface_list, "use_vlan" => use_vlan, "vlan" => vlan, "add_bridge" => add_bridge }
    node.crowbar["crowbar"]["network"][interface] = net_info
    node.save

    @logger.info("Network allocate_ip: Assigned: #{name} #{network} #{range} #{net_info["address"]}")
    [200, net_info]
  end

  def create_proposal
    @logger.debug("Network create_proposal: entering")
    base = super

    base["attributes"]["network"]["networks"].each do |k,net|
      @logger.debug("Network: creating #{k} in the network")
      bc = Chef::DataBagItem.new
      bc.data_bag "crowbar"
      bc["id"] = "#{k}_network"
      bc["network"] = net
      bc["allocated"] = {}
      bc["allocated_by_name"] = {}
      db = ProposalObject.new bc
      db.save
    end

    @logger.debug("Network create_proposal: exiting")
    base
  end

  def transition(inst, name, state)
    @logger.debug("Network transition: Entering #{name} for #{state}")

    if state == "discovered"
      db = ProposalObject.find_proposal "network", inst
      role = RoleObject.find_role_by_name "network-config-#{inst}"

      @logger.debug("Network transition: make sure that network role is on all nodes: #{name} for #{state}")
      result = add_role_to_instance_and_node("network", inst, name, db, role, "network")

      @logger.debug("Network transition: Exiting #{name} for #{state} discovered path")
      return [200, NodeObject.find_node_by_name(name).to_hash] if result
      return [400, "Failed to add role to node"] unless result
    end

    @logger.debug("Network transition: Exiting #{name} for #{state}")
    [200, NodeObject.find_node_by_name(name).to_hash]
  end

  def apply_role_pre_chef_call(old_role, role, all_nodes)
    @logger.debug("Network apply_role_pre_chef_call: entering #{all_nodes.inspect}")
    return if all_nodes.empty?

    old_mode = nil
    old_mode = old_role.default_attributes["network"]["mode"] unless old_role.nil? or old_role.default_attributes["network"].nil?
    new_mode = nil
    new_mode = role.default_attributes["network"]["mode"] unless role.nil? or role.default_attributes["network"].nil?

    return if old_mode == new_mode

    all_nodes.each do |n|
      node = NodeObject.find_node_by_name n

      save_it = false
      new_hash = {}
      node.crowbar["crowbar"]["network"].each do |intf, o_net_info|
        net_info = fix_interface(node, o_net_info, new_mode) 
        unless net_info.nil?
          save_it = true
          new_hash[net_info["interface"]] = net_info
        else
          new_hash[intf] = o_net_info
        end 
      end
      node.crowbar["crowbar"]["network"] = new_hash

      @logger.debug("Network apply_role_pre_chef_call: saving node") if save_it
      node.save if save_it
    end
    @logger.debug("Network apply_role_pre_chef_call: leaving")
  end

  def fix_interface(node, net_info, new_mode)
    @logger.debug("Network fix_interface: #{node.name} #{net_info.inspect} #{new_mode}")
    tanswer = get_interface_info node, net_info["usage"], new_mode, net_info["use_vlan"], net_info["vlan"]
    if !tanswer
      @logger.info("fix_interface: return failed to find interface: #{node.name}")
      return nil
    end
    interface, interface_list, mac = tanswer

    if interface != net_info["interface"]
      address = net_info["address"]

      begin # Rescue block
        f = acquire_ip_lock
        db = ProposalObject.find_data_bag_item "crowbar/#{net_info["usage"]}_network"
        db["allocated_by_name"][node.name] = { "machine" => node.name, "interface" => interface, "address" => address }
        db["allocated"][address] = { "machine" => node.name, "interface" => interface, "address" => address }
        db.save
      rescue Exception => e
        @logger.error("Error finding address: #{e.message}")
      ensure
        release_ip_lock(f)
      end

      net_info["interface"] = interface
      net_info["mac"] = mac
      net_info["interface_list"] = interface_list

      @logger.debug("Network fix_interface: leaving true: #{node.name} #{net_info.inspect} #{new_mode}")
      return net_info
    end

    @logger.debug("Network fix_interface: leaving false: #{node.name} #{net_info.inspect} #{new_mode}")
    return nil
  end

  def get_interface_info(node, network, mode, use_vlan, vlan)
    @logger.debug("Network get_interface_info: entering #{node.name} #{network} #{mode} #{use_vlan} #{vlan}")
    single = mode == "single"
    dual = mode == "dual"
    team = mode == "team"
    interface_list = node.interface_list

    if network == "bmc"
      interface = "bmc"
      mac = "bmc"
    else
      if single or ((network == "admin" or network == "bmc_vlan") and dual) or interface_list.size == 1
        linterface = interface = interface_list.first 
        interface_list = [ interface ]
        if use_vlan
          interface = "#{interface}.#{vlan}"
        end
      elsif dual
        linterface = interface = interface_list[1]
        interface_list = [ interface ]
        if use_vlan
          interface = "#{interface}.#{vlan}"
        end
      elsif team
        interface = "bond0"
        linterface = interface_list.first
        if use_vlan
          interface = "#{interface}.#{vlan}"
          interface_list = [ "bond0" ]
        end
      else
        return false
      end

      mac = ""
      node["network"]["interfaces"][linterface]["addresses"].each do |k,addr|
        mac = k.downcase
        break if addr[:family] == "lladdr"
      end
    end

    answer = [ interface, interface_list, mac ]
    @logger.debug("Network get_interface_info: leaving #{answer.inspect}")
    answer
  end

  def enable_interface(bc_instance, network, name)
    @logger.debug("Network enable_interface: entering #{name} #{network}")

    return [404, "No network specified"] if network.nil?
    return [404, "No name specified"] if name.nil?

    # Find the node
    node = NodeObject.find_node_by_name name
    @logger.error("Network enable_interface: return node not found: #{name} #{network}") if node.nil?
    return [404, "No node found"] if node.nil?

    # Find an interface based upon config
    role = RoleObject.find_role_by_name "network-config-#{bc_instance}"
    @logger.error("Network enable_interface: No network data found: #{name} #{network}") if role.nil?
    return [404, "No network data found"] if role.nil?

    mode = role.default_attributes["network"]["mode"]

    # If we already have on allocated, return success
    net_info = node.get_network_by_type(network)
    unless net_info.nil?
      intf = net_info["interface"]
      net_info = fix_interface(node, net_info, mode)
      unless net_info.nil?
        node.crowbar["crowbar"]["network"][intf] = nil
        node.crowbar["crowbar"]["network"][net_info["interface"]] = net_info
        node.save
      end
      @logger.error("Network enable_interface: node already has address: #{name} #{network}")
      return [200, net_info]
    end

    begin # Rescue block
      db = ProposalObject.find_data_bag_item "crowbar/#{network}_network"

      subnet = db["network"]["subnet"]
      vlan = db["network"]["vlan"]
      use_vlan = db["network"]["use_vlan"]
      add_bridge = db["network"]["add_bridge"]
      broadcast = db["network"]["broadcast"]
      router = db["network"]["router"]
      netmask = db["network"]["netmask"]

      tanswer = get_interface_info node, network, mode, use_vlan, vlan
      if !tanswer
        @logger.info("Network enable_interface: return failed to find interface: #{name} #{network} #{range}")
        return [404, "Failed to find interface"]
      end
      interface, interface_list, mac = tanswer
    rescue Exception => e
      @logger.error("Error finding address: #{e.message}")
    ensure
    end

    # Save the information.
    net_info = { "interface" => interface, "netmask" => netmask, "mac" => mac, "node" => name, "router" => router, "subnet" => subnet, "broadcast" => broadcast, "usage" => network, "interface_list" => interface_list, "use_vlan" => use_vlan, "vlan" => vlan, "add_bridge" => add_bridge }
    node.crowbar["crowbar"]["network"][interface] = net_info
    node.save

    @logger.info("Network enable_interface: Assigned: #{name} #{network}")
    [200, net_info]
  end

end
