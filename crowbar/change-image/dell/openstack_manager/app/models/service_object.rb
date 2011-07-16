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
#
# Also functions as a data bag item wrapper as well.
#
require 'chef'
require 'json'

class ServiceObject

  extend CrowbarOffline

  def self.all
    bc = {}
    ProposalObject.find("#{ProposalObject::BC_PREFIX}*").each do |bag|
      bc[bag.item.name[/#{ProposalObject::BC_PREFIX}(.*)/,1]] = bag.item[:description]
    end
    bc.delete_if { |k, v| bc.has_key? k[/^(.*)-(.*)/,0] }
    return bc
  end


  def acquire_lock(name)
    @logger.debug("Acquire #{name} lock enter")
    f = File.new("tmp/#{name}.lock", File::RDWR|File::CREAT, 0644)
    @logger.debug("Acquiring #{name} lock")
    rc = false
    count = 0
    while rc == false do
      count = count + 1
      @logger.debug("Attempt #{name} Lock: #{count}")
      rc = f.flock(File::LOCK_EX|File::LOCK_NB)
      sleep 1 if rc == false
    end
    @logger.debug("Acquire #{name} lock exit: #{f.inspect}, #{rc}")
    f
  end

  def release_lock(f)
    @logger.debug("Release lock enter: #{f.inspect}")
    f.flock(File::LOCK_UN)
    f.close
    @logger.debug("Release lock exit")
  end

  def queue_proposal(inst, bc = @bc_name)
    begin
      f = acquire_lock "queue"

      db = ProposalObject.find_data_bag_item "crowbar/queue"
      if db.nil?
        new_queue = Chef::DataBagItem.new
        new_queue.data_bag "crowbar"
        new_queue["id"] = "queue"
        new_queue["proposal_queue"] = []
        db = ProposalObject.new new_queue
      end

      db["proposal_queue"].each do |item|
        return if item["barclamp"] == bc and item["inst"] == inst
      end

      db["proposal_queue"] << { "barclamp" => bc, "inst" => inst }
      db.save
    rescue Exception => e
      @logger.error("Error queuing proposal for #{bc}:#{inst}: #{e.message}")
    ensure
      release_lock f
    end

    prop = ProposalObject.find_proposal(bc, inst)
    prop["deployment"][bc]["crowbar-queued"] = true
    prop.save
  end

  def dequeue_proposal(inst, bc = @bc_name)
    begin
      f = acquire_lock "queue"

      db = ProposalObject.find_data_bag_item "crowbar/queue"
      return if db.nil?

      db["proposal_queue"].delete_if { |item| item["barclamp"] == bc and item["inst"] == inst }
      db.save
      prop = ProposalObject.find_proposal(bc, inst)
      unless prop.nil?
        prop["deployment"][bc]["crowbar-queued"] = false
        prop.save

        remove_pending_elements(bc, inst, prop["deployment"][bc]["elements"])
      end
    rescue Exception => e
      @logger.error("Error dequeuing proposal for #{bc}:#{inst}: #{e.message}")
      return false
    ensure
      release_lock f
    end
    true
  end

  def process_queue
    list = []
    queue = []
    begin
      f = acquire_lock "queue"

      db = ProposalObject.find_data_bag_item "crowbar/queue"
      return if db.nil?

      queue = db["proposal_queue"]
    rescue Exception => e
      @logger.error("Error queuing proposal for #{bc}:#{inst}: #{e.message}")
    ensure
      release_lock f
    end

    # Test for ready
    queue.each do |item|
      prop = ProposalObject.find_proposal(item["barclamp"], item["inst"])
      dequeue_proposal(item["inst"], item["barclamp"]) if prop.nil?
      next if prop.nil?
      list << item if elements_not_ready(prop["deployment"][item["barclamp"]]["elements"]).empty?
    end

    # For each ready item, apply it.
    list.each do |item|
      bc = item["barclamp"]
      inst = item["inst"]
      service = eval("#{bc.capitalize}Service.new @logger")
      answer = service.proposal_commit(inst)
      dequeue_proposal(inst, bc) if answer[0] == 200
    end
  end

  def elements_not_ready(elements)
    # Get all the nodes
    all_new_nodes = []
    elements.each do |elem, nodes|
      all_new_nodes << nodes
    end
    all_new_nodes.flatten!

    # Check to see if we should delay our commit until nodes are ready.
    delay = []
    all_new_nodes.each do |n|
      node = NodeObject.find_node_by_name(n)
      next if node.nil?

      # if a node is not allocated, mark it as allocated
      unless node.allocated?
        node.allocated = true
        node.save
      end

      delay << n if node["state"] != "ready"
    end
    delay
  end

  def add_pending_elements(bc, inst, elements)
    # Create map with nodes and their element list
    all_new_nodes = {}
    elements.each do |elem, nodes|
      nodes.each do |node|
        all_new_nodes[node] = [] if all_new_nodes[node].nil?
        all_new_nodes[node] << elem
      end
    end

    # Add the entries to the nodes.
    all_new_nodes.each do |n, val|
      node = NodeObject.find_node_by_name(n)
      next if node.nil?

      node["crowbar"]["pending"] = {} if node["crowbar"]["pending"].nil?
      node["crowbar"]["pending"]["#{bc}-#{inst}"] = val
      node.save
    end
  end

  def remove_pending_elements(bc, inst, elements)
    # Create map with nodes and their element list
    all_new_nodes = {}
    elements.each do |elem, nodes|
      nodes.each do |node|
        all_new_nodes[node] = [] if all_new_nodes[node].nil?
        all_new_nodes[node] << elem
      end
    end

    # Remove the entries from the nodes.
    all_new_nodes.each do |n|
      node = NodeObject.find_node_by_name(n)
      next if node.nil?
      unless node["crowbar"]["pending"].nil? or node["crowbar"]["pending"]["#{bc}-#{inst}"].nil?
        node["crowbar"]["pending"]["#{bc}-#{inst}"] = {}
        node.save
      end
    end
  end

  def bc_name=(new_name)
    @bc_name = new_name
  end
  
  def bc_name 
    @bc_name
  end
  
  def initialize(thelogger)
    @bc_name = "unknown"
    @logger = thelogger
  end

  def versions
    [200, { :versions => [ "1.0" ] }]
  end

  def transition
    [200, {}]
  end

  def list_active
    roles = RoleObject.find_roles_by_name("#{@bc_name}-config-*")
    roles.map! { |r| r.name.gsub("#{@bc_name}-config-","") } unless roles.empty?
    [200, roles]
  end

  def show_active(inst)
    inst = "#{@bc_name}-config-#{inst}"

    role = RoleObject.find_role_by_name(inst)
    
    if role.nil?
      [404, "Active instance not found"]
    else
      [200, role]
    end
  end

  def clean_proposal(proposal)
    proposal.delete("controller")
    proposal.delete("action")
    proposal.delete("barclamp")
    proposal.delete("name")
    proposal.delete("_method")
    proposal.delete("authenticity_token")
  end

  #
  # Proposal is a json structure (not a ProposalObject)
  # Use to create or update an active instance
  #
  def active_update(proposal, inst)
    begin
      role = ServiceObject.proposal_to_role(proposal, @bc_name)
      clean_proposal(proposal)
      validate_proposal proposal
      apply_role(role, inst)
    rescue Net::HTTPServerException => e
      [e.response.code, {}]
    rescue Chef::Exceptions::ValidationFailed => e2
      [400, e2.message]
    end
  end

  def destroy_active(inst)
    inst = "#{@bc_name}-config-#{inst}"
    role = RoleObject.find_role_by_name(inst)
    if role.nil?
      [404, {}]
    else
      # By nulling the elements, it functions as a remove
      dep = role.override_attributes
      dep[@bc_name]["elements"] = {}
      role.override_attributes = dep
      answer = apply_role(role, inst)
      role.destroy
      answer
    end
  end

  def elements
    roles = RoleObject.find_roles_by_name("#{@bc_name}-*")
    cull_roles = RoleObject.find_roles_by_name("#{@bc_name}-config-*")
    roles.delete_if { |r| cull_roles.include?(r) } unless roles.empty?
    roles.map! { |r| r.name } unless roles.empty?
    [200, roles]
  end

  def element_info
    nodes = NodeObject.find_all_nodes
    nodes.map! { |n| n.name } unless nodes.empty?
    [200, nodes]
  end

  def proposals_raw
    ProposalObject.find_proposals(@bc_name)
  end 
  
  def proposals
    props = proposals_raw
    props.map! { |p| p["id"].gsub("bc-#{@bc_name}-", "") } unless props.empty?
    [200, props]
  end

  def proposal_show(inst)
    prop = ProposalObject.find_proposal(@bc_name, inst)
    if prop.nil?
      [404, {}]
    else
      [200, prop]
    end
  end

  #
  # This can be overridden to provide a better creation proposal
  #
  def create_proposal
    prop = ProposalObject.find_proposal("template", @bc_name)
    prop.raw_data
  end

  def proposal_create(params)
    base_id = params["id"]
    params["id"] = "bc-#{@bc_name}-#{params["id"]}"

    prop = ProposalObject.find_proposal(@bc_name, base_id)
    return [400, I18n.t('.name_exists', :scope=>'model.service')] unless prop.nil?
    return [400, I18n.t('.too_short', :scope=>'model.service')] if base_id.length == 0
    return [400, I18n.t('.illegal_chars', :scope=>'model.service')] if base_id =~ /[^A-Za-z0-9_]/

    base = create_proposal
    base["deployment"][@bc_name]["config"]["environment"] = "#{@bc_name}-config-#{base_id}"
    proposal = base.merge(params)
    clean_proposal(proposal)
    _proposal_update proposal
  end

  def proposal_edit(params)
    params["id"] = "bc-#{@bc_name}-#{params["id"]}"
    proposal = {}.merge(params)
    clean_proposal(proposal)
    _proposal_update proposal
  end

  def proposal_delete(inst)
    prop = ProposalObject.find_proposal(@bc_name, inst)
    if prop.nil?
      [404, {}]
    else
      prop.destroy
      [200, {}]
    end
  end

  def proposal_commit(inst)
    prop = ProposalObject.find_proposal(@bc_name, inst)

    if prop.nil?
      [404, "#{I18n.t('.cannot_find', :scope=>'model.service')}: #{@bc_name}.#{inst}"]
    elsif prop["deployment"][@bc_name]["crowbar-committing"]
      [402, "#{I18n.t('.already_commit', :scope=>'model.service')}: #{@bc_name}.#{inst}"]
    else
      # Put mark on the wall
      prop["deployment"][@bc_name]["crowbar-committing"] = true
      prop.save

      answer = active_update prop.raw_data, inst

      # Unmark the wall
      prop = ProposalObject.find_proposal(@bc_name, inst)
      prop["deployment"][@bc_name]["crowbar-committing"] = false
      prop.save

      answer
    end
  end

  #
  # This can be overridden to get better validation if needed.
  #
  def validate_proposal proposal
    path = "/opt/dell/chef/data_bags/crowbar"
    path = "schema" unless CHEF_ONLINE
    validator = CrowbarValidator.new("#{path}/bc-template-#{@bc_name}.schema")

    errors = validator.validate(proposal)
    if errors && !errors.empty?
      strerrors = ""
      errors.each do |e|
        strerrors += "#{e.message}\n"
      end
      raise Chef::Exceptions::ValidationFailed.new(strerrors)
    end
  end

  def _proposal_update(proposal)
    data_bag_item = Chef::DataBagItem.new

    begin 
      data_bag_item.raw_data = proposal
      data_bag_item.data_bag "crowbar"

      validate_proposal proposal

      prop = ProposalObject.new data_bag_item
      prop.save

      [200, {}]
    rescue Net::HTTPServerException => e
      [e.response.code, {}]
    rescue Chef::Exceptions::ValidationFailed => e2
      [400, e2.message]
    end
  end

  #
  # This is a role output function
  # Can take either a RoleObject or a Role.
  #
  def self.role_to_proposal(role, bc_name)
    proposal = {}

    proposal["id"] = role.name.gsub("#{bc_name}-config-", "bc-#{bc_name}-")
    proposal["description"] = role.description
    proposal["attributes"] = role.default_attributes
    proposal["deployment"] = role.override_attributes

    proposal
  end

  #
  # From a proposal json
  #
  def self.proposal_to_role(proposal, bc_name)
    role = Chef::Role.new
    role.name proposal["id"].gsub("bc-#{bc_name}-", "#{bc_name}-config-")
    role.description proposal["description"]
    role.default_attributes proposal["attributes"]
    role.override_attributes proposal["deployment"]
    RoleObject.new role
  end

  #
  # After validation, this is where the role is applied to the system
  # The old instance (if one exists) is compared with the new instance.
  # roles are removed and delete roles are added (if they exist) for nodes leaving roles
  # roles are added for nodes joining roles.
  # Calls chef-client on nodes
  #
  # This function can be overriden to define a barclamp specific operation.
  # A call is provided that receives the role and all string names of the nodes before the chef-client call
  #
  def apply_role(role, inst)
    # Query for this role
    old_role = RoleObject.find_role_by_name(role.name)

    nodes = {}

    # Get the new elements list
    new_deployment = role.override_attributes[@bc_name]
    new_elements = new_deployment["elements"]
    element_order = new_deployment["element_order"]

    delay = elements_not_ready(new_elements)
    unless delay.empty?
      add_pending_elements(@bc_name, inst, new_elements)
      queue_proposal(inst)
      return [202, delay]
    end

    # make sure the role is saved
    role.save

    # Build a list of old elements
    old_elements = {}
    old_deployment = old_role.override_attributes[@bc_name] unless old_role.nil?
    old_elements = old_deployment["elements"] unless old_deployment.nil?
    element_order = old_deployment["element_order"] if (!old_deployment.nil? and element_order.nil?)

    # Merge the parts based upon the element install list.
    all_nodes = []
    run_order = []
    element_order.each do | elems |
      r_nodes = []
      elems.each do |elem|
        old_nodes = old_elements[elem]
        new_nodes = new_elements[elem]

        unless old_nodes.nil?
          elem_remove = nil
          tmprole = RoleObject.find_role_by_name "#{elem}_remove"
          unless tmprole.nil?
            elem_remove = tmprole.name
          end

          old_nodes.each do |n|
            if new_nodes.nil? or !new_nodes.include?(n)
              nodes[n] = { :remove => [], :add => [] } if nodes[n].nil?
              nodes[n][:remove] << elem 
              nodes[n][:add] << elem_remove unless elem_remove.nil?
              r_nodes << n
            end
          end
        end

        unless new_nodes.nil?
          new_nodes.each do |n|
            all_nodes << n
            if old_nodes.nil? or !old_nodes.include?(n)
              nodes[n] = { :remove => [], :add => [] } if nodes[n].nil?
              nodes[n][:add] << elem
            end
            r_nodes << n unless r_nodes.include?(n)
          end
        end
      end
      run_order << r_nodes unless r_nodes.empty?
    end

    # Clean the run_lists
    admin_nodes = []
    nodes.each do |n, lists|
      node = NodeObject.find_node_by_name(n)
      next if node.nil?

      admin_nodes << n if node.admin?

      save_it = false

      rlist = lists[:remove]
      alist = lists[:add]

      # Remove the roles being lost
      rlist.each do |item|
        next unless node.role? item
        node.run_list.run_list_items.delete "role[#{item}]"
        save_it = true
      end

      # Add the roles being gained
      alist.each do |item|
        next if node.role? item
        node.run_list.run_list_items << "role[#{item}]"
        save_it = true
      end

      # Make sure the config role is on the nodes in this barclamp, otherwise remove it
      if all_nodes.include?(node.name)
        # Add the config role 
        unless node.role?(role.name)
          node.run_list.run_list_items << "role[#{role.name}]"
          save_it = true
        end
      else
        # Remove the config role 
        if node.role?(role.name)
          node.run_list.run_list_items.delete "role[#{role.name}]"
          save_it = true
        end
      end

      node.save if save_it
    end

    apply_role_pre_chef_call(old_role, role, all_nodes)

    # Each batch is a list of nodes that can be done in parallel.
    ran_admin = false
    run_order.each do | batch |
      next if batch.empty?
      snodes = ""
      admin_list = ""
      batch.each do |n|
        # Run admin nodes a different way.
        if admin_nodes.include?(n)
          admin_list = admin_list + " OR " if admin_list != ""
          admin_list = admin_list + "name:#{n}"
          ran_admin = true
          next
        end
        snodes = snodes + " OR " if snodes != ""
        snodes = snodes + "name:#{n}"
      end

      # Only take the actions if we are online
      if CHEF_ONLINE
        system("sudo -i -u root \"knife ssh '#{snodes}' chef-client\"")
        system("sudo -i -u root \"knife ssh '#{snodes}' chef-client\"")

        system("sudo -i -u root \"knife ssh '#{admin_list}' /opt/dell/bin/single_chef_client.sh\"") if admin_list != ""
        system("sudo -i -u root \"knife ssh '#{admin_list}' /opt/dell/bin/single_chef_client.sh\"") if admin_list != ""
      end
    end

    system("sudo /opt/dell/bin/single_chef_client.sh") if CHEF_ONLINE and !ran_admin
    [200, {}]
  end

  def apply_role_pre_chef_call(old_role, role, all_nodes)
    # noop by default.
  end

  def add_role_to_instance_and_node(barclamp, instance, name, prop, role, newrole)
    node = NodeObject.find_node_by_name name
    return false if node.nil?

    prop["deployment"][barclamp]["elements"][newrole] = [] if prop["deployment"][barclamp]["elements"][newrole].nil?
    unless prop["deployment"][barclamp]["elements"][newrole].include?(node.name)
      prop["deployment"][barclamp]["elements"][newrole] << node.name
      prop.save
    end

    role.override_attributes[barclamp]["elements"][newrole] = [] if role.override_attributes[barclamp]["elements"][newrole].nil?
    unless role.override_attributes[barclamp]["elements"][newrole].include?(node.name)
      role.override_attributes[barclamp]["elements"][newrole] << node.name
      role.save
    end

    save_it = false
    unless node.role?(newrole)
      node.run_list.run_list_items << "role[#{newrole}]"
      save_it = true
    end

    unless node.role?("#{barclamp}-config-#{instance}")
      node.run_list.run_list_items << "role[#{barclamp}-config-#{instance}]"
      save_it = true
    end

    node.save if save_it
    true
  end

end

