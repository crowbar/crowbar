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
# Author: RobHirschfeld 
# 
class NodeObject < ChefObject
  extend CrowbarOffline
  
  def self.find(search)
    answer = []
    if CHEF_ONLINE
      nodes = if search.nil?
        ChefObject.query_chef.search "node"
      else 
        ChefObject.query_chef.search "node", "#{chef_escape(search)}"
      end
      if nodes[2] != 0
        nodes[0].delete_if { |x| x.nil? }
        answer = nodes[0].map do |x| 
          dumped = self.dump x, 'node', x.name
          RoleObject.find_roles_by_search "name:#{make_role_name(x.name)}" if dumped
          NodeObject.new x
        end
      end
    else
      files = offline_search 'node-', ''
      answer = files.map! { |f| NodeObject.new(recover_json(f)) }
    end
    return answer
  end

  def self.find_all_nodes 
    self.find nil
  end

  def self.find_nodes_by_name(name)
    self.find "name:#{chef_escape(name)}"
  end

  def self.find_node_by_name(name)
    name += ".#{ChefObject.cloud_domain}" unless name =~ /(.*)\.(.)/
    val = if CHEF_ONLINE
      ChefObject.crowbar_node(name)
    else
      self.recover_json(self.nfile('node',name))
    end
    return val.nil? ? nil : NodeObject.new(val)
  end
  def self.all 
    self.find nil
  end

  def self.make_role_name(name)
    "crowbar-#{name.gsub(".", "_")}"
  end

  def self.create_new_role(new_name, machine)
    name = make_role_name new_name
    if CHEF_ONLINE
      role = RoleObject.new Chef::Role.new
    else
      self.create_object 'role', name, "NodeObject Create New Role"
      role = RoleObject.find_role_by_name(name)
    end
    role.name = name
    role.default_attributes["crowbar"] = {}
    role.default_attributes["crowbar"]["network"] = {} if role.default_attributes["crowbar"]["network"].nil?
    role.save

    machine.run_list.run_list_items << "role[#{role.name}]"
    machine.save

    role
  end

  def self.create_new(new_name)
    machine = Chef::Node.new
    machine.name "#{new_name}"
    machine["fqdn"] = "#{new_name}"
    role = RoleObject.find_role_by_name NodeObject.make_role_name(new_name)
    role = NodeObject.create_new_role(new_name, machine) if role.nil?
    NodeObject.new machine
  end

  def self.human_attribute_name(attrib)
    I18n.t attrib, :scope => "model.attributes.node"
  end
  
  def self.status(nodes)
    nodes.collect{|c| c.status}.inject(Hash.new(0)){|h,v| h[v] += 1; h}
  end
  
  def initialize(node)
    @role = RoleObject.find_role_by_name NodeObject.make_role_name(node.name)
    if @role.nil?
      # An admin node can exist without a role - so create one
      if !node["crowbar"].nil? and node["crowbar"]["admin_node"]
        @role = NodeObject.create_new_role(node.name, node)
      else
        Rails.logger.fatal("Node exists without role!! #{node.name}")
      end
    end
    @node = node
  end

  def has_node?
    !@node.nil?
  end

  def shortname
    name.split('.')[0]
  end

  def name
    @node.nil? ? 'unknown' : @node.name
  end
  
  def status
    case state
    when "ready"
      "ready"
    when "discovered"
      "pending"
    when "discovering", "reset", "delete", "reinstall", "shutdown", "reboot", "poweron", "noupdate"
      "unknown"
    else
      "unready"
    end
  end
  
  def ready?
    state === 'ready'
  end
  
  def state 
    return 'unknown' if @node.nil? 
    if self.crowbar['state'] === 'ready' and CHEF_ONLINE and @node['ohai_time']
      since_last = Time.now.to_i-@node['ohai_time'].to_i
      return 'noupdate' if since_last > 1200 # or 20 mins
    end
    return self.crowbar['state']
  end

  def ip
    net_info = get_network_by_type("admin")
    return net_info["address"] unless net_info.nil?
    @node["ipaddress"] || (I18n.t :unknown)
  end
  
  def public_ip
    net_info = get_network_by_type("public")
    return net_info["address"] unless net_info.nil?
    @node["ipaddress"] || (I18n.t :unknown)
  end
  
  def mac
    unless @node["crowbar"].nil? or @node["crowbar"]["switch_config"].nil?
      intf = sort_ifs[0]
      @node["crowbar"]["switch_config"][intf]["mac"] || (I18n.t :unknown)
    else
      (I18n.t :not_set)
    end
  end

  def allocated
    @node.nil? ? false : self.crowbar["crowbar"]["allocated"]
  end
  
  def allocated=(value)
    return false if @role.nil?
    self.crowbar["crowbar"]["allocated"] = value
  end
  
  def allocated?
    @node.nil? ? false : self.crowbar["crowbar"]["allocated"]
  end
  
  def ipmi_enabled?
    #placeholder until we have a better mechanism
    @node.nil? ? false : @node["crowbar"]["allocated"]
  end

  def rename(value, domain)
    return "unknown" if @node.nil?
    @node.name value
    @node[:fqdn] = value
    @node[:domain] = domain
    @node.run_list.run_list_items.delete "role[#{@role.name}]"
    @role.name = "crowbar-#{value.gsub(".", "_")}"
    @node.run_list.run_list_items << "role[#{@role.name}]"
    @node.save
    save
  end 

  def memory
    @node['memory']['total'] rescue nil
  end
  
  def cpu
    @node['cpu']['0']['model_name'].squeeze(" ").strip rescue nil
  end
  
  def uptime
    @node["uptime"]
  end
  
  def asset_tag
    @node["dmi"]["chassis"]["serial_number"] rescue nil
  end
  
  def number_of_drives
    self.crowbar['crowbar']['disks'].length rescue -1
  end

  def [](attrib)
    return nil if @node.nil?
    @node[attrib]
  end

  def crowbar_run_list(*args)
    return nil if @role.nil?
    args.length > 0 ? @role.run_list(args) : @role.run_list
  end

  def crowbar
    @role.default_attributes
  end

  def crowbar=(value)
    return nil if @role.nil?
    @role.default_attributes = value
  end

  def role?(role_name)
    return false if @node.nil?
    @role.run_list.run_list_items.each do |item|
      return true if item == "role[#{role_name}]"
    end
    @node.role?(role_name)
  end
  
  def roles
    @node['roles'].nil? ? nil : @node['roles'].sort
  end

  def recursive_merge!(b, h)
    b.merge!(h) {|key, _old, _new| if _old.class.kind_of? Hash.class then recursive_merge(_old, _new) else _new end  }
  end

  def save
    if @role.default_attributes["crowbar-revision"].nil?
      @role.default_attributes["crowbar-revision"] = 0
      Rails.logger.debug("Starting Node Revisions: #{@node.name} - unset")
    else
      Rails.logger.debug("Starting Node Revisions: #{@node.name} - #{@role.default_attributes["crowbar-revision"]}")
      @role.default_attributes["crowbar-revision"] = @role.default_attributes["crowbar-revision"] + 1
    end
    Rails.logger.debug("Saving node: #{@node.name} - #{@role.default_attributes["crowbar-revision"]}")

    recursive_merge!(@node.normal_attrs, @role.default_attributes)

    if CHEF_ONLINE
      @role.save
      @node.save
    else
      NodeObject.offline_cache(@node, NodeObject.nfile('node', @node.name))
      NodeObject.offline_cache(@role, RoleObject.nfile('role', @role.name))
    end
    Rails.logger.debug("Done saving node: #{@node.name} - #{@role.default_attributes["crowbar-revision"]}")
  end

  def destroy
    Rails.logger.debug("Destroying node: #{@node.name} - #{@role.default_attributes["crowbar-revision"]}")
    @role.destroy
    @node.destroy
    Rails.logger.debug("Done with removal of node: #{@node.name} - #{@role.default_attributes["crowbar-revision"]}")
  end

  def networks
    self.crowbar["crowbar"]["network"]
  end
  
  def get_network_by_type(type)
    return nil if @role.nil?
    networks.each do |intf, data|
      return data if data["usage"] == type
    end
    nil
  end

  #
  # This is from the crowbar role assigned to the admin node at install time.
  # It is not a node.role parameter
  #
  def admin?
    return false if @node.nil?
    return false if @node["crowbar"].nil?
    return false if @node["crowbar"]["admin_node"].nil?
    @node["crowbar"]["admin_node"]
  end

  def interface_list
    return [] if @node.nil?
    answer = []
    @node["network"]["interfaces"].each do |k,v|
      next if k == "lo"     # no loopback, please
      next if k =~ /^sit/   # Ignore sit interfaces
      next if k =~ /^vlan/  # Ignore nova create interfaces
      next if k =~ /^br/    # Ignore bridges interfaces
      next if k =~ /\.\d+/  # no vlan interfaces, please
      answer << k
    end
    answer
  end

  def adapter_count 
    interface_list.size
  end

  def bus_index(bus_order, path)
    return 999 if bus_order.nil?

    dpath = path.split(".")[0].split("/")

    index = 0
    bus_order.each do |b|
      subindex = 0
      bs = b.split(".")[0].split("/")

      match = true
      bs.each do |bp|
        break if subindex >= dpath.size
        match = false if bp != dpath[subindex]
        break unless match
        subindex = subindex + 1
      end

      return index if match
      index = index + 1
    end

    999
  end

  def get_bus_order
    bus_order = nil
    @node["network"]["interface_map"].each do |data|
      bus_order = data["bus_order"] if @node[:dmi][:system][:product_name] =~ /#{data["pattern"]}/
      break if bus_order
    end rescue nil
    bus_order
  end

  def sort_ifs
    bus_order = get_bus_order
    map = @node["crowbar"]["detected"]["network"]
    answer = map.sort{|a,b|
      aindex = bus_index(bus_order, a[1])
      bindex = bus_index(bus_order, b[1])
      aindex == bindex ? a[0] <=> b[0] : aindex <=> bindex
    }
    answer.map! { |x| x[0] }
  end

  # Switch config is actually a node set property from customer ohai.  It is really on the node and not the role
  def switch_name
    unless @node["crowbar"].nil? or @node["crowbar"]["switch_config"].nil?
      intf = sort_ifs[0]
      switch_name = @node["crowbar"]["switch_config"][intf]["switch_name"] || (I18n.t :undetermined)
      switch_name = (I18n.t :undetermined) if switch_name == -1
      switch_name.to_s.gsub(':', '-')
    else
      switch_name = (I18n.t :undetermined)
    end
  end
  
  def switch_port
    unless @node["crowbar"].nil? or @node["crowbar"]["switch_config"].nil?
      intf = sort_ifs[0]
      switch_name = @node["crowbar"]["switch_config"][intf]["switch_port"] || (I18n.t :undetermined)
    else
      switch_name = (I18n.t :undetermined)
    end
  end
  
  def location
    unless @node["crowbar"].nil? or @node["crowbar"]["switch_config"].nil?
      intf = sort_ifs[0]
      location = @node["crowbar"]["switch_config"][intf]["switch_port"] || (I18n.t :not_set)
    else
      location = (I18n.t :not_set)
    end
  end
  # Switch config is actually a node set property from customer ohai.  It is really on the node and not the role

  def description    
    @role.description.length==0 ? nil : @role.description
  end
  
  def description=(value)
    @role.description = value
  end
  
  def hardware
    @node["dmi"].system.product_name
  end

  def usage
    return [] if @role.nil?
    return [] if self.crowbar["crowbar"].nil?
    self.crowbar["crowbar"]["usage"]
  end

  def usage=(value)
    return [] if @role.nil?
    return [] if self.crowbar["crowbar"].nil?
    self.crowbar["crowbar"]["usage"] = value
  end
  
  def raid_set
    return nil if @role.nil?
    return nil if self.crowbar["crowbar"].nil?
    return nil if self.crowbar["crowbar"]["hardware"].nil?
    self.crowbar["crowbar"]["hardware"]["raid_set"]
  end

  def raid_set=(value)
    return nil if @role.nil?
    return nil if self.crowbar["crowbar"].nil?
    self.crowbar["crowbar"]["hardware"] = {} if self.crowbar["crowbar"]["hardware"].nil?
    self.crowbar["crowbar"]["hardware"]["raid_set"] = value
  end
  
  def bios_set
    return nil if @role.nil?
    return nil if self.crowbar["crowbar"].nil?
    return nil if self.crowbar["crowbar"]["hardware"].nil?
    self.crowbar["crowbar"]["hardware"]["bios_set"]
  end

  def bios_set=(value)
    return nil if @role.nil?
    return nil if self.crowbar["crowbar"].nil?
    self.crowbar["crowbar"]["hardware"] = {} if self.crowbar["crowbar"]["hardware"].nil?
    self.crowbar["crowbar"]["hardware"]["bios_set"] = value
  end
  
  def to_hash
    return {} if @node.nil?
    nhash = @node.to_hash
    rhash = @role.default_attributes.to_hash
    nhash.merge rhash
  end

  def set_state(state)
    if CHEF_ONLINE
      # use the real transition function for this
      cb = CrowbarService.new Rails.logger
      results = cb.transition "default", @node.name, state
    else
      puts "Node #{name} Chef State Changed to #{state}"
      self.crowbar['state'] = state
      save
    end

    if state == "reset" or state == "reinstall" or state == "update"
      if CHEF_ONLINE
        bmc = get_network_by_type("bmc")
        system("ipmitool -H #{bmc["address"]} -U crowbar -P crowbar power cycle") unless bmc.nil?
      else
        NodeObject.clear_cache @node
        puts "Node #{name} to #{state} caused cache object to be deleted."
      end
    end
    results
  end

  def reboot
    set_state("reboot")
    bmc = get_network_by_type("bmc")
    return puts "Node #{name} IMPI Reboot call to #{bmc["address"]}" unless CHEF_ONLINE
    system("ipmitool -H #{bmc["address"]} -U crowbar -P crowbar power cycle") unless bmc.nil?
  end

  def shutdown
    set_state("shutdown")
    bmc = get_network_by_type("bmc")
    if CHEF_ONLINE
      system("ipmitool -H #{bmc["address"]} -U crowbar -P crowbar power off") unless bmc.nil?
    else
      return puts "Node #{name} IMPI Shutdown call to #{bmc["address"]}" 
    end
  end

  def poweron
    set_state("poweron")
    bmc = get_network_by_type("bmc")
    if CHEF_ONLINE
      system("ipmitool -H #{bmc["address"]} -U crowbar -P crowbar power on") unless bmc.nil?
    else
      puts "Node #{name} IMPI Power On call to #{bmc["address"]}" 
    end
  end

  def power_change_state(state)
    self.crowbar['state'] = state
    save
  end

  def identify
    bmc = get_network_by_type("bmc")
    return puts "Node #{name} IMPI Identify call to #{bmc["address"]}" unless CHEF_ONLINE
    system("ipmitool -H #{bmc["address"]} -U crowbar -P crowbar chassis identify") unless bmc.nil?
  end

  def allocate
    return if @node.nil?
    return if @role.nil?
    self.allocated = true
    save
  end

  def bmc_set?
    return false if @node.nil? or @node["crowbar"].nil? or @node["crowbar"]["status"].nil?
    return false if @node["crowbar"]["status"]["ipmi"].nil?
    return false if @node["crowbar"]["status"]["ipmi"]["address_set"].nil?
    @node["crowbar"]["status"]["ipmi"]["address_set"]
  end

end


