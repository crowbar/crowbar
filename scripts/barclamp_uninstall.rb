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

ARGV.each do |src|
  component_dir = nil
  if Dir.exist?(src)
    component_dir = src
  elsif Dir.exist?(File.join(BARCLAMP_PATH, src))
    component_dir = File.join(BARCLAMP_PATH, src)
  else
    puts "#{src} is not a valid component name, skipping."
  end

  next if component_dir.nil?
  component = File.basename(component_dir)

  bc_remove_layout_1 from_rpm, component
  # TODO: bc_remove_layout_1_chef from_rpm, component
end

generate_navigation
generate_assets_manifest
catalog

exit 0
