#!/usr/bin/env ruby
#
# Copyright 2015, SUSE LINUX Products GmbH
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

require "getoptlong"
require "pp"
require "barclamp_mgmt_lib"

opts = GetoptLong.new(
  ["--help", "-h", GetoptLong::NO_ARGUMENT],
  ["--debug", "-d", GetoptLong::NO_ARGUMENT],
  ["--rpm", GetoptLong::NO_ARGUMENT]
)

def usage()
  puts "Usage:"
  puts "#{__FILE__} [--help] [--rpm] [--debug]"
  exit
end

from_rpm = false

opts.each do |opt, arg|
  case opt
  when "--help"
    usage
  when "--debug"
    ENV["DEBUG"] = "true"
    debug "debug mode is enabled"
  when "--rpm"
    from_rpm = true
  end
end

generate_assets_manifest

exit 0
