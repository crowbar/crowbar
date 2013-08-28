#!/usr/bin/ruby
# Copyright 2012, SUSE Linux Products GmbH
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
require 'getoptlong'
require 'yaml'

opts = GetoptLong.new(
  [ '--help', '-h', GetoptLong::NO_ARGUMENT ]
)

def usage()
  puts "Usage:"
  puts "#{__FILE__} [--help] [--debug] </path/to/new/crowbar.yml> </path/to/output.yml>"
  puts
  puts "    Use this tool to extract the 'locale_addtions' section"
  puts "    from </path/to/new/crowbar.yml> into to a separate yaml"
  puts "    file </path/to/output.yml>."
  puts "    Attention: The destitation file will be overwritten if"
  puts "    it already exists)"
  puts
  exit
end

opts.each do |opt, arg|
  case opt
    when "--help"
    usage
  end
end

usage if ARGV.length < 2

input_yaml = ARGV[0]
output_yaml = ARGV[1]

cb_yaml = YAML.load_file ARGV[0]

if cb_yaml.has_key? "locale_additions"
    File.open( output_yaml, 'w' ) do |out|
        YAML.dump( cb_yaml['locale_additions'], out )
    end
else
    puts "Info: #{input_yaml} has no section 'locale_additions'. Ignoring."
end


