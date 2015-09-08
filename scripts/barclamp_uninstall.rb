#!/usr/bin/env ruby
#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
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

$LOAD_PATH.unshift(File.dirname(__FILE__))

require 'getoptlong'
require 'barclamp_mgmt_lib'

opts = GetoptLong.new(
  [ '--help', '-h', GetoptLong::NO_ARGUMENT ],
  [ '--debug', '-d', GetoptLong::NO_ARGUMENT ],
  [ '--rpm', GetoptLong::NO_ARGUMENT ]
)

def usage()
  puts "Usage:"
  puts "#{__FILE__} [--help] [--rpm] [--debug] /path/to/old/barclamp"
  exit
end

from_rpm = false

opts.each do |opt, arg|
  case opt
    when "--help"
    usage
    when "--debug"
    ENV['DEBUG'] = 'true'
    debug "debug mode is enabled"
    when "--rpm"
    from_rpm = true
  end
end

usage if ARGV.length < 1

  # this is used by the install-chef installer script
  if __FILE__ == $0
    path = ARGV[0]
    debug "Using #{path}"

    barclamp_yml_files = Array.new

    barclamp_yml_files += get_yml_paths(path)
    if barclamp_yml_files.empty?
      path = File.join BARCLAMP_PATH, path
      barclamp_yml_files += get_yml_paths(path)
    end

    barclamp_yml_files.each do |yml_file|
      barclamp = YAML.load_file yml_file
      bc = barclamp["barclamp"]["name"].chomp.strip
      case barclamp["crowbar"]["layout"].to_i
      when 1
        bc_remove_layout_1 from_rpm, bc, path, barclamp
        #TODO: bc_remove_layout_1_chef bc, path, barclamp
      else
        puts "ERROR: could not Uninstall barclamp #{bc} because #{barclamp["barclamp"]["crowbar_layout"]} is unknown layout."
        exit -3
      end
    end
    #exit -3 if err
    exit 0
  end
