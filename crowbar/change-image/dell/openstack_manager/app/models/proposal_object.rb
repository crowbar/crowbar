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
class ProposalObject < ChefObject

  extend CrowbarOffline
  BC_PREFIX = 'bc-template-'
  
  def self.find_data_bag_item(bag)
    begin
      chef_init #elimiate
      bag = ProposalObject.new(Chef::DataBag.load bag)  #should use new syntax
      return bag
    rescue
      return nil
    end
  end
  
  def self.find(search)
    props = [] 
    begin
      if CHEF_ONLINE
        arr = ChefObject.query_chef.search("crowbar", "id:#{chef_escape(search)}") 
        if arr[2] != 0
          props = arr[0].map do |x| 
            self.dump x, 'data_bag_item_crowbar-bc', x.name[/bc-(.*)/,1] unless x.nil?
            ProposalObject.new x 
          end
          props.delete_if { |x| x.nil? or x.item.nil? }
        end
      else
        files = self.offline_search('data_bag_item_crowbar-', search.chop)
        props = files.map { |file| ProposalObject.new(self.recover_json(file)) }
      end
    rescue Exception => e
       Rails.logger.error("Could not recover Chef Crowbar data searching for '#{search}' due to '#{e.inspect}'")
    end
    return props
  end
    
  def self.all
    self.find 'bc-*'
  end
  
  def self.find_proposals(barclamp)
    self.find "bc-#{barclamp}-*"
  end

  def self.find_proposal(barclamp, name)
    self.find_proposal_by_id "bc-#{barclamp}-#{name}"
  end

  def self.find_proposal_by_id(id)
    val = if CHEF_ONLINE
      ChefObject.crowbar_data(id)
    else
      self.recover_json(self.nfile('data_bag_item_crowbar-bc', id[/bc-(.*)/,1]))
    end
    return val.nil? ? nil : ProposalObject.new(val)
  end

  def self.human_attribute_name(attrib)
    I18n.t attrib, :scope => "model.attributes.proposal"
  end

  def item
    @item
  end

  def id
    @item['id']
  end

  def name
    @item.name[/crowbar_bc-(.*)-(.*)$/,2]
  end
  
  def barclamp
    @item.name[/crowbar_bc-(.*)-(.*)$/,1]
  end

  def status
    bc = @item["deployment"][self.barclamp]
    if bc.nil?
      "unready"
    else
      return "unready" if bc.has_key? "crowbar-committing" and bc["crowbar-committing"]
      return "pending" if bc.has_key? "crowbar-queued" and bc["crowbar-queued"]
      "ready"
    end
  end
  
  def description
    @item['description']
  end
  
  def elements
    @item.raw_data['deployment'][self.barclamp]["elements"]
  end

  def all_elements
    @item.raw_data['deployment'][self.barclamp]["element_order"].flatten.uniq
  end
  
  def active?
    inst = "#{barclamp}-config-#{name}"
    role = RoleObject.find_role_by_name(inst)
    return role.nil?
  end

  def raw_data
    @item.raw_data
  end

  def raw_data=(value)
    @item.raw_data = value
  end

  def [](attrib)
    @item[attrib]
  end

  def []=(attrib, value)
    @item[attrib] = value
  end
  
  def initialize(x)
    @item = x
  end

  def save
    @item["deployment"] = {} if @item["deployment"].nil?
    @item["deployment"][barclamp] = {} if @item["deployment"][barclamp].nil?
    if @item["deployment"][barclamp]["crowbar-revision"].nil?
      @item["deployment"][barclamp]["crowbar-revision"] = 0
    else
      @item["deployment"][barclamp]["crowbar-revision"] = @item["deployment"][barclamp]["crowbar-revision"] + 1
    end
    Rails.logger.debug("Saving data bag item: #{@item["id"]} - #{@item["deployment"][barclamp]["crowbar-revision"]}")
    if CHEF_ONLINE
      @item.save
    else
      ProposalObject.offline_cache(@item, ProposalObject.nfile('data_bag_item_crowbar', @item.id))
    end
    Rails.logger.debug("Done saving data bag item: #{@item["id"]} - #{@item["deployment"][barclamp]["crowbar-revision"]}")
  end

  def destroy
    Rails.logger.debug("Destroying data bag item: #{@item["id"]} - #{@item["deployment"][barclamp]["crowbar-revision"]}")
    @item.destroy(@item.data_bag, @item["id"])
    Rails.logger.debug("Done removal of data bag item: #{@item["id"]} - #{@item["deployment"][barclamp]["crowbar-revision"]}")
  end
  
  private
  
  # 'array' is the unsorted set of objects
  # 'att_sym' is the symbol of the attribute each object in array, that is represented in index_array
  # 'index_array' is the ordered array of values
  def sort_with_index(array, att_sym, index_array)
    return array.sort do |a, b|
      index_array.index(a.send(att_sym)).to_i <=> index_array.index(b.send(att_sym)).to_i
    end
  end

end
