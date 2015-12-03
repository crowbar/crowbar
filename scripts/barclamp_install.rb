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
  [ '--force', '-f', GetoptLong::NO_ARGUMENT ],
  [ '--rpm', GetoptLong::NO_ARGUMENT ]
)

def usage()
  puts "Usage:"
  puts "#{__FILE__} [--help] [--rpm] [--debug] [--force] /path/to/new/barclamp"
  exit
end

force_install = false
from_rpm = false

opts.each do |opt, arg|
  case opt
    when "--help"
    usage
    when "--debug"
    ENV['DEBUG'] = 'true'
    debug "debug mode is enabled"
    when "--force"
    force_install = true
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
    barclamp_yml_files += get_yml_paths(src)
    component_paths.push src
  elsif Dir.exist?(File.join(BARCLAMP_PATH, src))
    barclamp_yml_files += get_yml_paths(File.join(BARCLAMP_PATH, src))
    component_paths.push File.join(BARCLAMP_PATH, src)
  else
    puts "#{src} is not a valid component name, skiping."
  end
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
    puts "Exception occured while parsing crowbar YAML file in #{yml_file}, skiping"
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

bc_install_layout_1_chef(from_rpm, component_paths, log)

debug "installing barclamps:"
barclamps.values.sort_by{|v| v[:order]}.each do |bc|
  debug "bc = #{bc.pretty_inspect}"
  begin
    unless bc[:src].start_with?(BARCLAMP_PATH)
      target=File.join(BARCLAMP_PATH, bc[:src].split("/")[-1])
      if File.directory? target
        debug "target directory #{target} exists"
        if get_yml_paths(target, bc[:name]).empty?
          debug "crowbar YAML file does not exists in #{target}"
          puts "#{target} exists, but it is not a barclamp."
          puts "Cowardly refusing to overwrite it."
          exit -1
        else
          debug "crowbar YAML file exists in #{target}"
          if File.exists? "#{target}/sha1sums"
            debug "#{target}/sha1sums file exists"
            unless force_install or system "cd \"#{target}\"; sha1sum --status -c sha1sums"
              debug "force_install mode is disabled and not all file checksums do match"
              print <<-EOF.gsub(/^ *\| /, '')
              | Refusing to install over non-pristine target #{target}
              | Please back up the following files:
              | #{`system "cd \"#{target}\""; sha1sum -c sha1sums |grep -v OK`}
              | and rerun the install after recreating the checksum file with:
              |   cd \"#{target}\"; find -type f -not -name sha1sums -print0 | \\
              |        xargs -0 sha1sum -b >sha1sums
              | (or use the --force switch)
              EOF
              exit -1
            end
          elsif not force_install
            debug "force_install mode is disabled and #{target}/sha1sums file does not exist"
            print <<-EOF.gsub(/^ *\| /, '')
            | #{target} already exists, but it does not have checksums.
            | Please back up any local changes you may have made, and then
            | create a checksums file with:
            |   cd \"#{target}\"; find -type f -not -name sha1sums -print0 | \\
            |       xargs -0 sha1sum -b >sha1sums
            | (or use the --force switch)
            EOF
            exit -1
          end
        end
      else
        debug "target directory \"#{target}\" does not exist"
        debug "creating directory \"#{target}\""
        system "mkdir -p \"#{target}\""
      end
      # Only rsync over the changes if this is a different install
      # from the POV of the sha1sums files
      unless File.exists?("#{bc[:src]}/sha1sums") and \
        File.exists?("#{target}/sha1sums") and \
        system "/bin/bash -c 'diff -q <(sort \"#{bc[:src]}/sha1sums\") <(sort \"#{target}/sha1sums\")'"
        debug "syncing \"#{bc[:src]}\" directory and \"#{target}\" directory"
        system "rsync -a \"#{bc[:src]}/\" \"#{target}\""
      end
      bc[:src] = target
    end
    debug "installing barclamp"
    begin
      if bc[:yaml]["crowbar"]["layout"].to_i == 1
        debug "Installing app components"
        bc_install_layout_1_app from_rpm, bc[:name], bc[:src], bc[:yaml]
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
