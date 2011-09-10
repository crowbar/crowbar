#!/usr/bin/ruby
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

  require File.join '/opt', 'dell', 'bin', 'barclamp_inst_lib'
  
  # this is used by the install-chef installer script 
  if __FILE__ == $0
    path = ARGV[0]
    puts "Using #{path}" if DEBUG
    barclamp = YAML.load_file File.join path, 'crowbar.yml'
    bc = barclamp["barclamp"]["name"].chomp.strip
    case barclamp["crowbar"]["layout"].to_i
    when 1
      bc_install_layout_1_app bc, path, barclamp
      bc_install_layout_1_chef bc, path, barclamp
    else
      puts "ERROR: could not install barclamp #{bc} because #{barclamp["barclamp"]["crowbar_layout"]} is unknown layout."
      exit -3
    end
    #TODO add bag verify
    #err = verify_bags( base_dir)
    #exit -3 if err
    exit 0
  end
  
