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
Crowbar::Application.routes.draw do

  resources :nodes, :only => [:index, :new] do
    get 'status', :on => :collection
  end
  
  scope 'nodes' do
    constraints(:name => /.*/ ) do
      match ':name/hit/:req' => "nodes#hit", :as => :hit_node
      match ':name/edit' => "nodes#edit", :as => :edit_node
      match ':name' => "nodes#show", :as => :node
      match ':name/update' => 'nodes#update', :as => :update_node
    end
    match 'status' => "nodes#status", :as => :nodes_status
  end
  
  match 'overview' => "overview#index"
  
  scope 'crowbar' do
    version = "1.0"

    match "show/#{version}/:id", :controller => 'barclamp', :action => 'barclamp_show', :via => :get, :as => :barclamp_show_barclamp
    match "roles/#{version}", :controller => 'barclamp', :action => 'barclamp_roles', :via => :get, :as => :barclamp_roles_barclamp
    match "proposals/#{version}", :controller => 'barclamp', :action => 'barclamp_proposals', :via => :get, :as => :barclamp_proposals_barclamp
    match "proposals/#{version}", :controller => 'barclamp', :action => 'proposal_status', :via => :get, :as => :status_proposals_barclamp

    match ":controller/#{version}/help", :action => 'help', :via => :get, :as => :help_barclamp
    match ":controller/#{version}/proposals", :action => 'proposal_create', :via => :put, :as => :create_proposal_barclamp
    match ":controller/#{version}/proposals", :action => 'proposals', :via => :get, :as => :proposals_barclamp
    match ":controller/#{version}/proposals/commit/:id", :action => 'proposal_commit', :via => :post, :as => :commit_proposal_barclamp
    match "#{version}/proposals/status.:format", :controller=>'barclamp', :action => 'proposal_status', :via => :get, :as => :status_proposals
    match ":controller/#{version}/proposals/:id", :action => 'proposal_delete', :via => :delete, :as => :delete_proposal_barclamp
    match ":controller/#{version}/proposals/:id", :action => 'proposal_update', :via => :post, :as => :update_proposal_barclamp
    match ":controller/#{version}/proposals/:id", :action => 'proposal_show', :via => :get, :as => :proposal_barclamp
    match ":controller/#{version}/elements", :action => 'elements', :via => :get
    match ":controller/#{version}/elements/:id", :action => 'element_info', :via => :get
    match ":controller/#{version}/transition/:id", :action => 'transition', :via => [:get, :post]
    match ":controller/#{version}", :action => 'index', :via => :get, :as => :index_barclamp
    match "#{version}/status", :controller => 'barclamp', :action => 'proposal_status', :via => :get, :as => :status_barclamp
    match ":controller/#{version}/:id", :action => 'delete', :via => :delete, :as => :delete_barclamp
    match ":controller/#{version}/:id", :action => 'show', :via => :get, :as => :show_barclamp
    match ":controller/#{version}/:action/:id", :via => :post, :as => :action_barclamp
    match ":controller", :action => 'versions', :via => :get, :as => :versions_barclamp
            
    # Generic fall through routes
    match ":barclamp/#{version}/help", :action => 'help', :via => :get, :controller => 'barclamp'
    match ":barclamp/#{version}/proposals", :action => 'proposal_create', :via => :put, :controller => 'barclamp'
    match ":barclamp/#{version}/proposals", :action => 'proposals', :via => :get, :controller => 'barclamp'
    match ":barclamp/#{version}/proposals/commit/:id", :action => 'proposal_commit', :via => :post, :controller => 'barclamp'
    match "#{version}/proposals/status.:format", :controller => 'barclamp', :action => 'proposal_status', :via => :get, :controller => 'barclamp'
    match ":barclamp/#{version}/proposals/:id", :action => 'proposal_delete', :via => :delete, :controller => 'barclamp'
    match ":barclamp/#{version}/proposals/:id", :action => 'proposal_update', :via => :post, :controller => 'barclamp'
    match ":barclamp/#{version}/proposals/:id", :action => 'proposal_show', :via => :get, :controller => 'barclamp'
    match ":barclamp/#{version}/elements", :action => 'elements', :via => :get, :controller => 'barclamp'
    match ":barclamp/#{version}/elements/:id", :action => 'element_info', :via => :get, :controller => 'barclamp'
    match ":barclamp/#{version}/transition/:id", :action => 'transition', :via => [:get, :post], :controller => 'barclamp'
    match ":barclamp/#{version}", :action => 'index', :via => :get, :controller => 'barclamp'
    match ":barclamp/#{version}/status", :action => 'status', :via => :get, :controller => 'barclamp'
    match ":barclamp/#{version}/:id", :action => 'delete', :via => :delete, :controller => 'barclamp'
    match ":barclamp/#{version}/:id", :action => 'show', :via => :get, :controller => 'barclamp'
    match ":barclamp/#{version}/:action/:id", :via => :post, :controller => 'barclamp'
    match ":barclamp", :action => 'versions', :via => :get, :controller => 'barclamp'

    match "/", :controller => 'barclamp', :action => 'barclamp_index', :via => :get, :as => :barclamp_index_barclamp
  end

  root :to => "nodes#index"  
  # match ':controller(/:action(/:id(.:format)))'

end
