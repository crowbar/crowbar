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

class MachinesController < ApplicationController

  self.help_contents = Array.new(superclass.help_contents)

  def index
    if FileTest.exist? CHEF_CLIENT_KEY
      begin
        if session[:domain].nil? 
          session[:domain] = ChefObject.cloud_domain
        end
        @app = NodeObject.find_all_nodes
      rescue
        flash.now[:notice] = "ERROR: Could not connect to Chef Server at \"#{CHEF_SERVER_URL}.\""
        @app = []
      end
    else
      flash.now[:notice] = "ERROR: Could not find Chef Key at \"#{CHEF_CLIENT_KEY}.\""
    end

    respond_to do |format|
      format.html
      format.json { render :json => @app.map! { |x| x.name } }
    end
  end

  add_help(:list)
  def list
    index
  end

  add_help(:show,[:name])
  def show
    name = params[:name]
    if session[:domain].nil? 
      session[:domain] = ChefObject.cloud_domain
    end
    name = "#{name}.#{session[:domain]}" if name.split(".").length <= 1

    machine = NodeObject.find_node_by_name name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not node for name #{name}"
      respond_to do |format|
        format.html
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      respond_to do |format|
        format.html
        format.json { render :json => machine.to_hash }
      end
    end
  end

  add_help(:reinstall,[:name],[:post])
  def reinstall
    name = params[:name]
    name = "#{name}.#{session[:domain]}" if name.split(".").length <= 1

    machine = NodeObject.find_node_by_name name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not node for name #{name}"
      respond_to do |format|
        format.html
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.set_state("reinstall")
      respond_to do |format|
        format.html { redirect_to :action => :index }
        format.json { render :json => {} }
      end
    end
  end

  add_help(:reset,[:name],[:post])
  def reset
    name = params[:name]
    name = "#{name}.#{session[:domain]}" if name.split(".").length <= 1

    machine = NodeObject.find_node_by_name name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not node for name #{name}"
      respond_to do |format|
        format.html
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.set_state("reset")
      respond_to do |format|
        format.html { redirect_to :action => :index }
        format.json { render :json => {} }
      end
    end
    redirect_to :action => :index
  end

  add_help(:identify,[:name],[:post])
  def identify
    name = params[:name]
    name = "#{name}.#{session[:domain]}" if name.split(".").length <= 1

    machine = NodeObject.find_node_by_name name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not node for name #{name}"
      respond_to do |format|
        format.html
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.identify
      respond_to do |format|
        format.html { redirect_to :action => :index }
        format.json { render :json => {} }
      end
    end
  end

  add_help(:shutdown,[:name],[:post])
  def shutdown
    name = params[:name]
    name = "#{name}.#{session[:domain]}" if name.split(".").length <= 1

    machine = NodeObject.find_node_by_name name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not node for name #{name}"
      respond_to do |format|
        format.html
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.shutdown
      respond_to do |format|
        format.html { redirect_to :action => :index }
        format.json { render :json => {} }
      end
    end
  end

  add_help(:reboot,[:name],[:post])
  def reboot
    name = params[:name]
    name = "#{name}.#{session[:domain]}" if name.split(".").length <= 1

    machine = NodeObject.find_node_by_name name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not node for name #{name}"
      respond_to do |format|
        format.html
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.reboot
      respond_to do |format|
        format.html { redirect_to :action => :index }
        format.json { render :json => {} }
      end
    end
  end

  add_help(:poweron,[:name],[:post])
  def poweron
    name = params[:name]
    name = "#{name}.#{session[:domain]}" if name.split(".").length <= 1

    machine = NodeObject.find_node_by_name name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not node for name #{name}"
      respond_to do |format|
        format.html
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.poweron
      respond_to do |format|
        format.html { redirect_to :action => :index }
        format.json { render :json => {} }
      end
    end
  end

  add_help(:allocate,[:name],[:post])
  def allocate
    name = params[:name]
    name = "#{name}.#{session[:domain]}" if name.split(".").length <= 1

    machine = NodeObject.find_node_by_name name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not node for name #{name}"
      respond_to do |format|
        format.html
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.allocate
      respond_to do |format|
        format.html { redirect_to :action => :index }
        format.json { render :json => {} }
      end
    end
  end

  add_help(:delete,[:name],[:delete])
  def delete
    name = params[:name]
    name = "#{name}.#{session[:domain]}" if name.split(".").length <= 1

    machine = NodeObject.find_node_by_name name
    if machine.nil?
      flash.now[:notice] = "ERROR: Could not node for name #{name}"
      respond_to do |format|
        format.html
        format.json { render :text => "Host not found", :status => 404 }
      end
    else
      machine.set_state("delete")
      respond_to do |format|
        format.html { redirect_to :action => :index }
        format.json { render :json => {} }
      end
    end
  end


end
