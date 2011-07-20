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
class NodesController < ApplicationController

  require 'chef'
  
  # GET /nodes
  # GET /nodes.xml
  def index
    @sum = 0
    if params.has_key?(:role)
      result = NodeObject.all #this is not efficient, please update w/ a search!
      @nodes = result.find_all { |node| node.role? params[:role] }
      if params.has_key?(:names_only)
         names = @nodes.map { |node| node.shortname }
         @nodes = {:role=>params[:role], :nodes=>names, :count=>names.count}
      end
    else
      @nodes = NodeObject.all
      get_node_and_network(params[:selected]) if params[:selected]
      flash[:notice] = "<b>#{t :warning, :scope => :error}:</b> #{t :no_nodes_found, :scope => :error}" if @nodes.empty? #.html_safe if @nodes.empty?
      @nodes.each do |node|
        @sum = @sum + node.name.hash
      end
    end
    respond_to do |format|
      format.html # index.html.erb
      format.xml  { render :xml => @nodes }
      format.json { render :json => @nodes }
    end
  end

  def status
    nodes = {}
    switches = {}
    sum = 0
    begin
      result = NodeObject.all
      result.each do |node|      
        nodes[node.shortname] = {:status=>node.status, :state=>(I18n.t node.state, :scope => :state)}
        count = switches[node.switch_name] || {"ready"=>0, "pending"=>0, "unready"=>0, "unknown"=>0}
        count[node.status] += 1
        switches[node.switch_name] = count
        sum = sum + node.name.hash
      end
      render :inline => {:sum => sum, :nodes=>nodes, :switches=>switches, :count=>nodes.length}.to_json, :cache => false
    rescue Exception=>e
      count = (e.class.to_s == "Errno::ECONNREFUSED" ? -2 : -1)
      Rails.logger.fatal("Failed to iterate over node list due to '#{e.message}'")
      render :inline => {:nodes=>nodes, :switches=>switches, :count=>count, :error=>e.message}, :cache => false
    end
  end
  
  def hit
    action = params[:req]
    name = params[:name] || params[:id]
    machine = NodeObject.find_node_by_name name
    if machine.nil?
      render :text=>"Could not find node '#{name}'", :status => 404
    else
      case action
      when 'reinstall', 'reset', 'update', 'delete'
        machine.set_state(action)
      when 'reboot'
        machine.reboot
      when 'shutdown'
        machine.shutdown
      when 'poweron'
        machine.poweron
      when 'identify'
        machine.identify
      when 'allocate'
        machine.allocate
      else 
        render :text=>"Invalid hit requeset '#{action}'", :status => 500
      end
    end
    render :text=>"Attempting '#{action}' for node '#{machine.name}'", :status => 200
  end

  # GET /nodes/1
  # GET /nodes/1.xml
  def show
    get_node_and_network(params[:id] || params[:name])
    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @node }
      format.json { render :json => @node }
    end
  end
  
  def edit
    get_node_and_network(params[:id] || params[:name])
  end

  def update
    if request.post?
      get_node_and_network(params[:id] || params[:name])
      if params[:submit] == t('nodes.form.allocate')
        @node.allocated = true
        save_node
        flash[:notice] = t('nodes.form.allocate_node_success')
      elsif params[:submit] == t('nodes.form.save')
        save_node
        flash[:notice] = t('nodes.form.save_node_success')
      else
        Rails.logger.warn "Unknown action for node edit: #{params[:submit]}"
        flash[:notice] = "Unknown action: #{params[:submit]}"
      end
    else
      Rails.logger.warn "PUT is required to update proposal #{params[:id]}"
      flash[:notice] = "PUT required"
    end
    redirect_to nodes_path(:selected => @node.name)
  end
  
  private

  def save_node
    @node.usage = [ params[:usage] ]
    @node.bios_set = params[:bios]
    @node.raid_set = params[:raid]
    @node.description = params[:description]
    @node.save
  end
  
  def get_node_and_network(node_name)
    @network = {}
    notconns = {}
    @node = NodeObject.find_node_by_name(node_name)
    if @node
      # build network information (this may need to move into the object)
      interfaces = @node.interface_list.find_all { |n| n=~/^eth/ }
      @node.networks.each do |intf, data|
        @network[data["usage"]] = {} if @network[data["usage"]].nil?
        @network[data["usage"]][intf] = data["address"] || 'n/a'
        interfaces.delete(intf)
      end
      interfaces.map { |i| notconns[i] = nil }
    end
    @network['[not managed]'] = notconns
    @network.sort
  end
end
