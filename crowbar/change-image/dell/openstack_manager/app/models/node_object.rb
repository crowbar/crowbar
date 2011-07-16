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
        answer = nodes[0].map do |x| 
          self.dump x, 'node', x.name unless x.nil?
          NodeObject.new x
        end
        answer.delete_if { |x| x.nil? or x.node.nil? }
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

  def self.create_new(new_name, new_domain, new_mac)
    machine = Chef::Node.new
    machine["crowbar"] = {}
    machine["crowbar"]["node-id"] = new_mac
    machine["crowbar"]["network"] = {} if machine["crowbar"]["network"].nil?
    machine["crowbar"]["decoration"] = { "name" => new_name }
    machine["fqdn"] = "#{new_name}.#{new_domain}"
    machine.name "#{new_name}.#{new_domain}"

    NodeObject.new machine
  end

  def self.human_attribute_name(attrib)
    I18n.t attrib, :scope => "model.attributes.node"
  end
  
  def self.status(nodes)
    nodes.collect{|c| c.status}.inject(Hash.new(0)){|h,v| h[v] += 1; h}
  end
  
  def initialize(node)
    @node = node
  end

  def shortname
    name.split('.')[0]
  end

  def node
    @node
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
    if @node['state'] === 'ready' and CHEF_ONLINE
      since_last = Time.now.to_i-@node['ohai_time'].to_i
      return 'noupdate' if since_last > 1200 # or 20 mins
    end
    return @node['state']
  end

  def ip
    net_info = get_network_by_type("admin")
    return net_info["address"] unless net_info.nil?
    node["ipaddress"] || (I18n.t :unknown)
  end
  
  def public_ip
    net_info = get_network_by_type("public")
    return net_info["address"] unless net_info.nil?
    node["ipaddress"] || (I18n.t :unknown)
  end
  
  def mac
    net_info = get_network_by_type("admin")
    if net_info.nil?
      (I18n.t :not_set)
    else
      net_info["mac"] || (I18n.t :unknown)
    end
  end

  def allocated
    @node.nil? ? false : @node["crowbar"]["allocated"]
  end
  
  def allocated=(value)
    return false if @node.nil?
    @node["crowbar"]["allocated"] = value
  end
  
  def allocated?
    @node.nil? ? false : @node["crowbar"]["allocated"]
  end

  def name=(value)
    return "unknown" if @node.nil?
    @node.name value
  end 

  def memory
    node['memory']['total'] rescue nil
  end
  
  def cpu
    node['cpu']['0']['model_name'].squeeze(" ").strip rescue nil
  end
  
  def uptime
    node["uptime"]
  end
  
  def asset_tag
    node["dmi"]["chassis"]["serial_number"]  
  end
  
  def number_of_drives
    node['crowbar']['disks'].length rescue -1
  end

  def [](attrib)
    return nil if @node.nil?
    @node[attrib]
  end

  def []=(attrib, value)
    return nil if @node.nil?
    @node[attrib] = value
  end

  def run_list(*args)
    return nil if @node.nil?
    args.length > 0 ? @node.run_list(args) : @node.run_list
  end

  def role?(role_name)
    return false if @node.nil?
    @node.role?(role_name)
  end
  
  def roles
    @node['roles'].nil? ? nil : @node['roles'].sort
  end

  def save
    if @node["crowbar-revision"].nil?
      @node["crowbar-revision"] = 0
      Rails.logger.debug("Starting Node Revisions: #{@node.name} - unset")
    else
      Rails.logger.debug("Starting Node Revisions: #{@node.name} - #{@node["crowbar-revision"]}")
      @node["crowbar-revision"] = @node["crowbar-revision"] + 1
    end
    Rails.logger.debug("Saving node: #{@node.name} - #{@node["crowbar-revision"]}")
    if CHEF_ONLINE
      @node.save
    else
      NodeObject.offline_cache(@node, NodeObject.nfile('node', @node.name))
    end
    Rails.logger.debug("Done saving node: #{@node.name} - #{@node["crowbar-revision"]}")
  end

  def destroy
    Rails.logger.debug("Destroying node: #{@node.name} - #{@node["crowbar-revision"]}")
    @node.destroy
    Rails.logger.debug("Done with removal of node: #{@node.name} - #{@node["crowbar-revision"]}")
  end

  def get_network_by_type(type)
    return nil if @node.nil?
    @node["crowbar"]["network"].each do |intf, data|
      return data if data["usage"] == type
    end
    nil
  end

  def get_network_by_interface(intf)
    return nil if @node.nil?
    @node["crowbar"]["network"][intf]
  end

  def admin?
    return false if @node.nil?
    @node["crowbar"]["admin_node"]
  end

  def interface_list
    return [] if @node.nil?
    answer = []
    @node["network"]["interfaces"].each do |k,v|
      next if k == "lo"     # no loopback, please
      next if k =~ /^sit/   # Ignore sit interfaces
      next if k =~ /\.\d+/  # no vlan interfaces, please
      answer << k
    end
    answer
  end
  def switch_name
    unless node["crowbar"].nil? or node["crowbar"]["switch_config"].nil?
      switch_name = node["crowbar"]["switch_config"]["eth0"]["switch_name"] || (I18n.t :undetermined)
      switch_name = (I18n.t :undetermined) if switch_name == -1
      switch_name.to_s.gsub(':', '-')
    else
      switch_name = (I18n.t :undetermined)
    end
  end
  
  def switch_port
    unless node["crowbar"].nil? or node["crowbar"]["switch_config"].nil?
      switch_name = node["crowbar"]["switch_config"]["eth0"]["switch_port"] || (I18n.t :undetermined)
    else
      switch_name = (I18n.t :undetermined)
    end
  end
  
  def location
    unless node["crowbar"].nil? or node["crowbar"]["switch_config"].nil?
      location = node["crowbar"]["switch_config"]["eth0"]["switch_port"] || (I18n.t :not_set)
    else
      location = (I18n.t :not_set)
    end
  end

  def description
    unless node["crowbar"].nil? or node["crowbar"]["description"].nil?
      node["crowbar"]["description"] || (I18n.t :not_set)
    else
      I18n.t :not_set
    end
  end
  
  def description=(value)
     node["crowbar"]["description"] = value
  end
  
  def hardware
    node["dmi"].system.product_name
  end

  def usage
    return [] if @node.nil?
    return [] if @node["crowbar"].nil?
    @node["crowbar"]["usage"]
  end

  def usage=(value)
    return [] if @node.nil?
    return [] if @node["crowbar"].nil?
    @node["crowbar"]["usage"] = value
  end
  
  def raid_set
    return nil if @node.nil?
    return nil if @node["crowbar"].nil?
    return nil if @node["crowbar"]["hardware"].nil?
    @node["crowbar"]["hardware"]["raid_set"]
  end

  def raid_set=(value)
    return nil if @node.nil?
    return nil if @node["crowbar"].nil?
    @node["crowbar"]["hardware"] = {} if @node["crowbar"]["hardware"].nil?
    @node["crowbar"]["hardware"]["raid_set"] = value
  end
  
  def bios_set
    return nil if @node.nil?
    return nil if @node["crowbar"].nil?
    return nil if @node["crowbar"]["hardware"].nil?
    @node["crowbar"]["hardware"]["bios_set"]
  end

  def bios_set=(value)
    return nil if @node.nil?
    return nil if @node["crowbar"].nil?
    @node["crowbar"]["hardware"] = {} if @node["crowbar"]["hardware"].nil?
    @node["crowbar"]["hardware"]["bios_set"] = value
  end
  
  def to_hash
    return {} if @node.nil?
    @node.to_hash
  end

  def set_state(state)
    if CHEF_ONLINE
      # use the real transition function for this
      cb = CrowbarService.new Rails.logger
      results = cb.transition "default", @node.name, state
    else
      puts "Node #{name} Chef State Changed to #{state}"
      @node['state'] = state
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
    @node['state'] = state
    @node.save
  end

  def identify
    bmc = get_network_by_type("bmc")
    return puts "Node #{name} IMPI Identify call to #{bmc["address"]}" unless CHEF_ONLINE
    system("ipmitool -H #{bmc["address"]} -U crowbar -P crowbar chassis identify") unless bmc.nil?
  end

  def allocate
    return if @node.nil?
    self.allocated = true
    @node.save
  end

end


