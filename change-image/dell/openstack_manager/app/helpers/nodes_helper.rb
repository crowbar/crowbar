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
module NodesHelper
  
  def roles_list(roles)
    return [] if roles.nil? or roles.empty?
    roles.delete_if { |role| role =~ /^.*-config-/ }
    (roles.map {|role| "<a href='#{nodes_path({:role=>role, :names_only=>true, :format=>'json'})}'>#{role}</a>"} * ', ') #.html_safe
  end

  def barclamps_list(roles)
    return [] if roles.nil? or roles.empty?
    roles.delete_if { |role| !(role =~ /^.*-config-/) }
    (roles.map {|role| "<a href='#{nodes_path({:role=>role, :names_only=>true, :format=>'json'})}'>#{role.gsub("-config-", " ").titlecase}</a>"} * ', ') #.html_safe
  end
  
  def format_memory(kB)
    mem = (kB.to_f / 1024 / 1024)
    "#{sprintf("%#1.2f", mem)} GB"
  end
  
  def ip_addresses(ip_list)
    html = ""
    ip_list.each_pair do |network, addresses|
      unless network=='~notconnected' && addresses.nil?
        html += "<li><b>#{network}:</b> #{addresses.keys.collect {|k| "#{k}: #{addresses[k]}"}.join(',')}</li>"
      end
    end
    html
  end
  
end
