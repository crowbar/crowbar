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
class RoleObject < ChefObject

  extend CrowbarOffline

  def self.all
    self.find_roles_by_search(nil)
  end
  
  def self.find_roles_by_name(name)
    roles = []
    if CHEF_ONLINE
      #TODO this call could be moved to fild_roles_by_search
      arr = ChefObject.query_chef.search "role", "name:#{chef_escape(name)}"
      if arr[2] != 0
        roles = arr[0].map { |x| RoleObject.new x }
        roles.delete_if { |x| x.nil? or x.role.nil? }
      end
    else
      roles = find_roles_by_search name
    end
    roles
  end

  def self.find_roles_by_search(search)
    roles = []
    if CHEF_ONLINE
      arr = if search.nil?
        ChefObject.query_chef.search "role"
      else
        ChefObject.query_chef.search "role", search
      end
      if arr[2] != 0
        roles = arr[0].map { |x| RoleObject.new x }
        roles.delete_if { |x| x.nil? or x.role.nil? }
        arr[0].each { |role| self.dump role, 'role', role.name unless role.nil? }
      end
    else
      files = offline_search 'role-', search
      roles = files.map! { |f| RoleObject.new(recover_json(f)) }
    end
    roles
  end

  def self.find_role_by_name(name)
    if CHEF_ONLINE
      begin
        chef_init
        return RoleObject.new Chef::Role.load(name)
      rescue
        return nil
      end
    else
      answer = self.recover_json(self.nfile('role',name))
      return answer.nil? ? nil : RoleObject.new(answer)
    end
  end

  def self.human_attribute_name(attrib)
    I18n.t attrib, :scope => "model.attributes.role"
  end

  def barclamp
    @role.name.split("-")[0]
  end

  def inst
    @role.name.gsub("#{self.barclamp}-config-", "")
  end

  def role
    @role
  end

  def name
    @role.name
  end

  def name=(value)
    @role.name value
  end

  def description
    @role.description
  end

  def description=(value)
    @role.description value
  end

  def default_attributes
    @role.default_attributes
  end

  def default_attributes=(value)
    @role.default_attributes value
  end

  def override_attributes
    @role.override_attributes
  end

  def override_attributes=(value)
    @role.override_attributes value
  end

  def initialize(x)
    @role = x
  end

  def save
    @role.override_attributes[barclamp] = {} if @role.override_attributes[barclamp].nil?
    if @role.override_attributes[barclamp]["crowbar-revision"].nil?
      @role.override_attributes[barclamp]["crowbar-revision"] = 0
    else
      @role.override_attributes[barclamp]["crowbar-revision"] = @role.override_attributes[barclamp]["crowbar-revision"] + 1
    end
    Rails.logger.debug("Saving role: #{@role.name} - #{@role.override_attributes[barclamp]["crowbar-revision"]}")
    if CHEF_ONLINE
      @role.save
    else
      RoleObject.offline_cache(@role, RoleObject.nfile('role', @role.name))
    end
    Rails.logger.debug("Done saving role: #{@role.name} - #{@role.override_attributes[barclamp]["crowbar-revision"]}")
  end

  def destroy
    Rails.logger.debug("Destroying role: #{@role.name} - #{@role.override_attributes[barclamp]["crowbar-revision"]}")
    @role.destroy
    Rails.logger.debug("Done removing role: #{@role.name} - #{@role.override_attributes[barclamp]["crowbar-revision"]}")
  end

  def elements
    @role.override_attributes[self.barclamp]["elements"]
  end

  def run_list
    @role.run_list
  end

end

