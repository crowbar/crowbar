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
require 'chef'
require 'json'

class BarclampController < ApplicationController

  before_filter :controller_to_barclamp

  def controller_to_barclamp
    @bc_name = params[:barclamp] || params[:controller]
    @service_object.bc_name = @bc_name
  end

  self.help_contents = Array.new(superclass.help_contents)
  def initialize
    super()
    @service_object = ServiceObject.new logger
  end

  # Barclamp List (generic)
  add_help(:barclamp_index)
  def barclamp_index
    @barclamps = ServiceObject.all

    respond_to do |format|
      format.html { render :template => 'barclamp/barclamp_index' }
      format.xml  { render :xml => @barclamps }
      format.json { render :json => @barclamps }
    end
  end

  # Barclamp Show (specific one)
  add_help(:barclamp_show, [:id])
  def barclamp_show
    @barclamp = ServiceObject.new logger
    @barclamp.bc_name = params[:id]

    respond_to do |format|
      format.html { render :template => 'barclamp/barclamp_show' }
      format.xml  { render :xml => @proposal }
      format.json
    end
  end

  # Barclamp Roles (all)
  add_help(:barclamp_roles, [])
  def barclamp_roles
    @roles = RoleObject.all
    @roles = @roles.delete_if { |v| v.nil? or !(v.name =~ /^.*-config-/) }
    respond_to do |format|
      format.html { render :template => 'barclamp/barclamp_roles' }
      format.xml  { render :xml => @roles }
      format.json { render :json => @roles }
    end
  end

  # Barclamp Proposals (all)
  add_help(:barclamp_proposals, [])
  def barclamp_proposals
    @proposals = ProposalObject.all
    @proposals = @proposals.delete_if { |v| v.nil? or v.id =~ /^#{ProposalObject::BC_PREFIX}/ }
    respond_to do |format|
      format.html { render :template => 'barclamp/barclamp_proposals' }
      format.xml  { render :xml => @proposals }
      format.json { render :json => @proposals }
    end
  end

  add_help(:versions)
  def versions
    ret = @service_object.versions
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  add_help(:transition, [:id, :name, :state], [:get,:post])
  def transition
    id = params[:id]       # Provisioner id
    state = params[:state] # State of node transitioning
    name = params[:name] # Name of node transitioning

    ret = @service_object.transition(id, name, state)
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  add_help(:index)
  def index
    ret = @service_object.list_active
    @roles = ret[1]
    respond_to do |format|
      format.html { 
        @roles.map! { |r| RoleObject.find_role_by_name("#{@bc_name}-config-#{r}") }
        render :template => 'barclamp/index' 
      }
      format.xml  { 
        return render :text => @roles, :status => ret[0] if ret[0] != 200
        render :xml => @roles 
      }
      format.json {
        return render :text => @roles, :status => ret[0] if ret[0] != 200
        render :json => @roles 
      }
    end
  end

  add_help(:show,[:id])
  def show
    ret = @service_object.show_active params[:id]
    @role = ret[1]
    respond_to do |format|
      format.html { render :template => 'barclamp/show' }
      format.xml  { 
        return render :text => @role, :status => ret[0] if ret[0] != 200
        render :xml => ServiceObject.role_to_proposal(@role, @bc_name)
      }
      format.json { 
        return render :text => @role, :status => ret[0] if ret[0] != 200
        render :json => ServiceObject.role_to_proposal(@role, @bc_name)
      }
    end
  end

  add_help(:delete,[:id],[:delete])
  def delete
    params[:id] = params[:id] || params[:name]
    ret = [500, "Server Problem"]
    begin
      ret = @service_object.destroy_active(params[:id])
      flash[:notice] = ret[1] if ret[0] >= 300
      flash[:notice] = t('barclamp.show.delete_role_success') if ret[0] == 200
    rescue Exception => e
      flash[:notice] = e.message
    end

    respond_to do |format|
      format.html {
        return redirect_to show_barclamp_path(:id => params[:id], :controller => @bc_name) if ret[0] != 200
        redirect_to barclamp_roles_barclamp_path
      }
      format.xml  { 
        return render :text => ret[1], :status => ret[0] if ret[0] != 200
        render :xml => {}
      }
      format.json { 
        return render :text => ret[1], :status => ret[0] if ret[0] != 200
        render :json => {}
      }
    end
  end

  add_help(:status,[],[:get])
  def status
    bcs = {}
    begin
      result = ProposalObject.all
      result = result.delete_if { |v| v.id =~ /^#{ProposalObject::BC_PREFIX}/ }
      result.each do |role|
        bcs[role.barclamp] = (bcs[role.barclamp].nil? ? 1 : bcs[role.barclamp] + 1)
      end
      render :inline => {:count=>bcs.length, :barclamps=>bcs}.to_json, :cache => false
    rescue Exception=>e
      count = (e.class.to_s == "Errno::ECONNREFUSED" ? -2 : -1)
      Rails.logger.fatal("Failed to iterate over proposals list due to '#{e.message}'")
    end
  end

  add_help(:elements)
  def elements
    ret = @service_object.elements
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  add_help(:element_info,[:id])
  def element_info
    ret = @service_object.element_info
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  add_help(:proposals)
  def proposals
    ret = @service_object.proposals
    @proposals = ret[1]
    return render :text => @proposals, :status => ret[0] if ret[0] != 200
    respond_to do |format|
      format.html { 
        @proposals.map! { |p| ProposalObject.find_proposal(@bc_name, p) }
        render :template => 'barclamp/proposal_index' 
      }
      format.xml  { render :xml => @proposals }
      format.json { render :json => @proposals }
    end
  end

  add_help(:proposal_show,[:id])
  def proposal_show
    ret = @service_object.proposal_show params[:id]
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    @proposal = ret[1]

    @attr_raw = params[:attr_raw] || false
    @dep_raw = params[:dep_raw] || false

    respond_to do |format|
      format.html { render :template => 'barclamp/proposal_show' }
      format.xml  { render :xml => @proposal.raw_data }
      format.json { render :json => @proposal.raw_data }
    end
  end

  add_help(:proposal_status,[],[:get])
  def proposal_status
    proposals = {}
    begin
      result = ProposalObject.all
      result = result.delete_if { |v| v.id =~ /^#{ProposalObject::BC_PREFIX}/ }
      result.each do |prop|
        proposals["#{prop.barclamp}_#{prop.name}"] = prop.status
      end
      render :inline => {:proposals=>proposals, :count=>proposals.length}.to_json, :cache => false
    rescue Exception=>e
      count = (e.class.to_s == "Errno::ECONNREFUSED" ? -2 : -1)
      Rails.logger.fatal("Failed to iterate over proposal list due to '#{e.message}'")
      # render :inline => {:proposals=>proposals, :count=>count, :error=>e.message}, :cache => false
    end
  end

  add_help(:proposal_create,[:name],[:put])
  def proposal_create
    Rails.logger.fatal "Proposal Create starting. Params #{params.to_s}"    
    controller = params[:controller]
    orig_id = params[:name] || params[:id]
    params[:id] = orig_id
    answer = [ 500, "Server issue" ]
    begin
      Rails.logger.info "asking for proposal of: #{params}"
      answer = @service_object.proposal_create params
      Rails.logger.info "proposal is: #{answer}"
      flash[:notice] =  answer[0] != 200 ? answer[1] : t('barclamp.proposal_show.create_proposal_success')
    rescue Exception => e
      flash[:notice] = e.message
    end
    respond_to do |format|
      format.html { 
        return redirect_to barclamp_show_barclamp_path(controller) if answer[0] != 200
        redirect_to proposal_barclamp_path(:controller => controller, :id => orig_id) 
      }
      format.xml  {
        return render :text => flash[:notice], :status => answer[0] if answer[0] != 200
        render :xml => answer[1] 
      }
      format.json {
        return render :text => flash[:notice], :status => answer[0] if answer[0] != 200
        render :json => answer[1] 
      }
    end
  end

  add_help(:proposal_update,[:id],[:post])
  def proposal_update
    if params[:submit].nil?  # This is RESTFul path
      ret = @service_object.proposal_edit params
      return render :text => ret[1], :status => ret[0] if ret[0] != 200
      return render :json => ret[1]
    else # This is UI.
      params[:id] = "bc-#{params[:barclamp]}-#{params[:name]}"
      if params[:submit] == t('barclamp.proposal_show.save_proposal')
        @proposal = ProposalObject.find_proposal_by_id(params[:id])

        begin
          @proposal["attributes"][params[:barclamp]] = JSON.parse(params[:proposal_attributes])
          @proposal["deployment"][params[:barclamp]] = JSON.parse(params[:proposal_deployment])

          @service_object.validate_proposal @proposal.raw_data
          @proposal.save
          flash[:notice] = t('barclamp.proposal_show.save_proposal_success')
        rescue Exception => e
          flash[:notice] = e.message
        end
      elsif params[:submit] == t('barclamp.proposal_show.commit_proposal')
        @proposal = ProposalObject.find_proposal_by_id(params[:id])
 
        begin
          @proposal["attributes"][params[:barclamp]] = JSON.parse(params[:proposal_attributes])
          @proposal["deployment"][params[:barclamp]] = JSON.parse(params[:proposal_deployment])

          @service_object.validate_proposal @proposal.raw_data
          @proposal.save

          answer = @service_object.proposal_commit(params[:name])
          flash[:notice] = answer[1] if answer[0] >= 300
          flash[:notice] = t('barclamp.proposal_show.commit_proposal_success') if answer[0] == 200
          flash[:notice] = "#{t('barclamp.proposal_show.commit_proposal_queued')}: #{answer[1].inspect}" if answer[0] == 202
        rescue Exception => e
          flash[:notice] = e.message
        end
      elsif params[:submit] == t('barclamp.proposal_show.delete_proposal')
        begin
          answer = @service_object.proposal_delete(params[:name])
          flash[:notice] = answer[1] if answer[0] >= 300
          flash[:notice] = t('barclamp.proposal_show.delete_proposal_success') if answer[0] == 200
          return redirect_to barclamp_proposals_barclamp_path if answer[0] == 200
        rescue Exception => e
          flash[:notice] = e.message
        end
      elsif params[:submit] == t('barclamp.proposal_show.dequeue_proposal')
        begin
          answer = @service_object.dequeue_proposal(params[:name])
          flash[:notice] = t('barclamp.proposal_show.dequeue_proposal_failure') unless answer
          flash[:notice] = t('barclamp.proposal_show.dequeue_proposal_success') if answer
        rescue Exception => e
          flash[:notice] = e.message
        end
      else
        Rails.logger.warn "Invalid action #{params[:submit]} for #{params[:id]}"
        flash[:notice] = "Invalid action #{params[:submit]}"
      end
      redirect_to proposal_barclamp_path(:controller => params[:barclamp], :id => params[:name]) 
    end
  end

  add_help(:proposal_delete,[:id],[:delete])
  def proposal_delete
    ret = @service_object.proposal_delete params[:id]
    return render :text => ret[1], :status => ret[0] if ret[0] != 200
    render :json => ret[1]
  end

  add_help(:proposal_commit,[:id],[:post])
  def proposal_commit
    ret = @service_object.proposal_commit params[:id]
    return render :text => ret[1], :status => ret[0] if ret[0] >= 210
    render :json => ret[1], :status => ret[0]
  end

  add_help(:proposal_dequeue,[:id],[:post])
  def proposal_dequeue
    ret = @service_object.dequeue_proposal params[:id]
    return render :text => "Failed to dequeue", :status => 400 unless ret
    render :json => {}, :status => 200 if ret
  end

end

