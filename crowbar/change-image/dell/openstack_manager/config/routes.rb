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
ActionController::Routing::Routes.draw do |map|

  map.root :controller => "nodes", :action=>'index'

  map.resources :nodes, :only => [:index, :new]
  map.nodes_status 'nodes/status.:format', :controller => 'nodes', :action => 'status', :conditions => { :method => :get }

  map.hit_node 'nodes/:id/hit/:req', :controller=>'nodes', :action=>'hit', :constraints => { :name => /.*/ }
  map.edit_node 'nodes/:name/edit', :controller=>'nodes', :action =>'edit', :constraints => { :name => /.*/ }
  map.node 'nodes/:name', :controller => 'nodes', :action => 'show', :constraints => { :name => /.*/ }
  map.update_node 'nodes/:name/update', :controller => 'nodes', :action=>'update', :constraints => { :name => /.*/ }
  
  map.overview 'overview.:format', :controller => 'overview', :action => 'index'

  map.help_barclamp             'crowbar/:controller/1.0/help', :action => 'help', :conditions => { :method => :get }
  map.create_proposal_barclamp  'crowbar/:controller/1.0/proposals', :action => 'proposal_create', :conditions => { :method => :put }
  map.proposals_barclamp        'crowbar/:controller/1.0/proposals', :action => 'proposals', :conditions => { :method => :get }
  map.commit_proposal_barclamp  'crowbar/:controller/1.0/proposals/commit/:id', :action => 'proposal_commit', :conditions => { :method => :post }
  map.status_proposals_barclamp 'crowbar/:controller/1.0/proposals/status.:format', :action => 'proposal_status', :conditions => { :method => :get }
  map.delete_proposal_barclamp  'crowbar/:controller/1.0/proposals/:id', :action => 'proposal_delete', :conditions => { :method => :delete }
  map.update_proposal_barclamp  'crowbar/:controller/1.0/proposals/:id', :action => 'proposal_update', :conditions => { :method => :post }
  map.proposal_barclamp         'crowbar/:controller/1.0/proposals/:id', :action => 'proposal_show', :conditions => { :method => :get }
  map.connect 'crowbar/:controller/1.0/elements', :action => 'elements', :conditions => { :method => :get }
  map.connect 'crowbar/:controller/1.0/elements/:id', :action => 'element_info', :conditions => { :method => :get }
  map.connect 'crowbar/:controller/1.0/transition/:id', :action => 'transition', :conditions => { :method => :post }
  map.connect 'crowbar/:controller/1.0/transition/:id', :action => 'transition', :conditions => { :method => :get }
  map.index_barclamp            'crowbar/:controller/1.0', :action => 'index', :conditions => { :method => :get }
  map.status_barclamp           'crowbar/:controller/1.0/status', :action => 'status', :conditions => { :method => :get }
  map.delete_barclamp           'crowbar/:controller/1.0/:id', :action => 'delete', :conditions => { :method => :delete }
  map.show_barclamp             'crowbar/:controller/1.0/:id', :action => 'show', :conditions => { :method => :get }
  map.versions_barclamp         'crowbar/:controller', :action => 'versions', :conditions => { :method => :get }
  map.action_barclamp           'crowbar/:controller/1.0/:action/:id', :conditions => { :method => :post }
  map.barclamp_index_barclamp   'crowbar', :controller => 'barclamp', :action => 'barclamp_index', :conditions => { :method => :get }
  map.barclamp_show_barclamp    'crowbar/show/1.0/:id', :controller => 'barclamp', :action => 'barclamp_show', :conditions => { :method => :get }
  map.barclamp_roles_barclamp   'crowbar/roles/1.0', :controller => 'barclamp', :action => 'barclamp_roles', :conditions => { :method => :get }
  map.barclamp_proposals_barclamp 'crowbar/proposals/1.0', :controller => 'barclamp', :action => 'barclamp_proposals', :conditions => { :method => :get }

  map.connect 'crowbar/:barclamp/1.0/help', :action => 'help', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals', :action => 'proposal_create', :conditions => { :method => :put }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals', :action => 'proposals', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals/commit/:id', :action => 'proposal_commit', :conditions => { :method => :post }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals/status.:format', :action => 'proposal_status', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals/:id', :action => 'proposal_delete', :conditions => { :method => :delete }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals/:id', :action => 'proposal_update', :conditions => { :method => :post }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals/:id', :action => 'proposal_show', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/elements', :action => 'elements', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/elements/:id', :action => 'element_info', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/transition/:id', :action => 'transition', :conditions => { :method => :post }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/transition/:id', :action => 'transition', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0', :action => 'index', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/status', :action => 'status', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/:id', :action => 'delete', :conditions => { :method => :delete }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/:id', :action => 'show', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp', :action => 'versions', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/:action/:id', :conditions => { :method => :post }, :controller => 'barclamp'

  map.connect ':controller/:action/:id'
  map.connect ':controller/:action/:id.:format'
end
