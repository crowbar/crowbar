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

class UbuntuInstallService < ServiceObject
  
  def initialize(thelogger)
    @bc_name = "ubuntu_install"
    @logger = thelogger
  end
  
  
  def create_proposal
    @logger.info(" ubuntu_install create_proposal: entering")
    base = super
    @logger.info("ubuntu_install create_proposal: exiting. base prop is #{base.to_hash}")
    base
  end
  
  
  def transition(inst, name, state)
    return unless node["crowbar"]["hardware"]["os"] == "ubuntu" 
    @logger.debug("ubuntu_install transition: entering for #{name} for #{state}")
    @inst = inst
    @node_name = name
    @state = state
    result = true
    node = NodeObject.find_node_by_name(name)
    
    ### there's a chicken and egg issue here:
    ### on client side, the ubuntu-install barclamp needs to be executed early (to setup APT and such)
    ### because of the early execution, on admin/provisioner the ubuntu-install ends up executing befure
    ### before the provisioner role got added to the admin node.
    ### To resolve that, on the admin, rather than acting in "discovered", the action is taken on a later transition ("hardware-installed") 
    if state == "hardware-installed"          
      ## make sure the ubuntu server side components are installed on the provisioner node
      if node.role?("provisioner-server") 
        add_role "ubuntu_install"         
      end      
    end
    
    if state == "discovered" and
      add_role "ubuntu_base"
    end
    @logger.debug("ubuntu_install transaction: done. ")
    return [200,node.to_hash]
  rescue    
    @logger.error("ubuntu_install transaction: existing for #{name} for #{state}: Failed")
    return [400, "Failed to add role to node"]
  end
  
  
  def add_role (role_name)
    return unless  node["crowbar"]["hardware"]["os"] == "ubuntu" 
    msg = "ubuntu_install transaction: add #{role_name} to #{@node_name}"
    @logger.debug(msg)
    db = ProposalObject.find_proposal "ubuntu_install", @inst
    raise "cant find proposal " if db.nil?
    crole = RoleObject.find_role_by_name "ubuntu_install-config-#{@inst}"
    raise "cant find config-role" if crole.nil?
    result = add_role_to_instance_and_node("ubuntu_install", @inst, @node_name, db, crole, role_name)
    if !result
      msg = "FAILED #{msg}"
      @logger.fatal(msg)
      raise msg 
    end
    result      
  end  
end


