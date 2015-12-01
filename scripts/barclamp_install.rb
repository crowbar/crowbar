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
require 'pp'
require 'barclamp_mgmt_lib'

opts = GetoptLong.new(
  [ '--help', '-h', GetoptLong::NO_ARGUMENT ],
  [ '--debug', '-d', GetoptLong::NO_ARGUMENT ],
  [ '--rpm', GetoptLong::NO_ARGUMENT ]
)

def usage()
  puts "Usage:"
  puts "#{__FILE__} [--help] [--rpm] [--debug] /path/to/new/barclamp"
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

log_path = File.join '/var', 'log', 'crowbar'
FileUtils.mkdir log_path unless File.directory? log_path
log = File.join log_path, "component_install.log"

barclamp_yml_files = Array.new
component_paths = Array.new
ARGV.each do |src|
  debug "src: #{src}"
  if Dir.exist?(src)
    component_dir = src
  elsif Dir.exist?(File.join(BARCLAMP_PATH, src))
    component_dir = File.join(BARCLAMP_PATH, src)
  else
    puts "#{src} is not a valid component name, skipping."
    component_dir = nil
  end

  next if component_dir.nil?

  barclamp_yml_files += get_yml_paths(component_dir)
  component_paths.push component_dir
end

if barclamp_yml_files.blank?
  puts "No valid crowbar components found."
  exit -1
end

debug "checking components"

barclamps = Hash.new
barclamp_yml_files.each do |yml_file|
  begin
    debug "trying to parse crowbar YAML file in #{yml_file}"
    barclamp = YAML.load_file yml_file
  rescue
    puts "Exception occured while parsing crowbar YAML file in #{yml_file}, skipping"
    next
  end

  if barclamp["barclamp"] and barclamp["barclamp"]["name"]
    name = barclamp["barclamp"]["name"]
  else
    puts "Barclamp at #{yml_file} has no name, skipping"
    next
  end

  # We assume the barclamp and crowbar keys exist and their values are hashes.
  version = (barclamp["barclamp"]["version"] || 0).to_i
  order   = (barclamp["crowbar"]["order"] || 9999).to_i

  barclamps[name] = { :src => File.dirname(yml_file), :name => name, :order => order, :yaml => barclamp, :version => version }
  debug "barclamp[#{name}] = #{barclamps[name].pretty_inspect}"
end

barclamps.values.sort_by{|v| v[:order]}.each do |bc|
  debug "Check barclamp versions of #{bc[:name]}"
  if bc[:yaml]["nav"] && bc[:version] < 1
    fatal("Refusing to install #{bc[:name]} barclamp version < 1 due to incompatible navigation.", nil, -1)
  end
  debug "Check migration of #{bc[:name]}"
  bc[:migrate] = check_schema_migration(bc[:name])
end

component_paths.each do |component_path|
  bc_install_layout_1_app(from_rpm, component_path)
end
bc_install_layout_1_chef(from_rpm, component_paths, log)

debug "installing barclamps:"
barclamps.values.sort_by{|v| v[:order]}.each do |bc|
  debug "bc = #{bc.pretty_inspect}"
  begin
    debug "installing barclamp"
    begin
      if bc[:yaml]["crowbar"]["layout"].to_i == 1
        debug "Installing app components"
        bc_install_layout_1_chef_migrate bc[:name], log if bc[:migrate]
      else
        debug "Could not install barclamp #{bc[:name]} because #{bc[:yaml][:barclamp][:crowbar_layout]} is unknown layout."
      end
    rescue StandardError => e
      debug "exception occurred while installing barclamp"
      raise e
    end
  rescue StandardError => e
    puts e
    puts e.backtrace
    puts "Install of #{bc[:name]} failed."
    exit -3
  end
end

generate_navigation
generate_assets_manifest
catalog

exit 0
