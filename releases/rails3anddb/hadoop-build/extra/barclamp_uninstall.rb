#!/usr/bin/env ruby
# Copyright 2011, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Author: RobHirschfeld
#

  # the 1st choice is to use the code from the framework since it is most up to date
  # however, that code is not always available when installing

require '/opt/dell/bin/barclamp_mgmt_lib.rb'
    
  # this is used by the install-chef installer script 
  if __FILE__ == $0
    path = ARGV[0]
    debug "Using #{path}"
    bc_file = File.join path, 'crowbar.yml'
    unless File.exist? bc_file
      path = File.join '/opt', 'dell', 'barclamps', path
      bc_file = File.join path, 'crowbar.yml'
    end
    barclamp = YAML.load_file bc_file
    bc = barclamp["barclamp"]["name"].chomp.strip
    case barclamp["crowbar"]["layout"].to_i
    when 1
      bc_remove_layout_1 bc, path, barclamp
      #TODO: bc_remove_layout_1_chef bc, path, barclamp
    else
      puts "ERROR: could not UNinstall barclamp #{bc} because #{barclamp["barclamp"]["crowbar_layout"]} is unknown layout."
      exit -3
    end
    #exit -3 if err
    exit 0
  end
 
