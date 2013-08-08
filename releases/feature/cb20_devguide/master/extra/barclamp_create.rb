#!/usr/bin/env ruby
# Copyright 2012, Dell
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

require 'rubygems'
require 'yaml'
require 'json'
require 'fileutils'
require 'pp'

MODEL_SUBSTRING_BASE = '==BC-MODEL=='
MODEL_SUBSTRING_CAMEL = '==^BC-MODEL=='
MODEL_SUBSTRING_HUMAN = '==*BC-MODEL=='
MODEL_SUBSTRING_CAPSS = '==%BC-MODEL=='

barclamp_dir='/opt/dell/barclamps'

if ENV["CROWBAR_DIR"]
  barclamp_dir="#{ENV["CROWBAR_DIR"]}/barclamps"
end

def camelize(str)
  str.split('_').map {|w| w.capitalize}.join
end

# remove model placeholders
def bc_replacer(item, bc, entity)
  debug "bc_replacer method called with debug option enabled"
  debug "bc_replacer args: item=#{item}, bc=#{bc}, entity=#{entity}"

  item.gsub!(MODEL_SUBSTRING_BASE, bc)
  item.gsub!(MODEL_SUBSTRING_CAMEL, bc.camelize)
  item.gsub!(MODEL_SUBSTRING_HUMAN, bc.humanize)
  item.gsub!(MODEL_SUBSTRING_CAPSS, bc.capitalize)
  item.gsub!('Copyright 2012, Dell', "Copyright #{Time.now.year}, #{entity}")
  debug "bc_replacer returns item=#{item}"
  return item
end

# copies paths from one place to another (recursive)
def bc_cloner(item, bc, entity, source, target, replace)
  # Replace: ONLY used when creating a new barclamp from the model!
  debug "bc_cloner method called with debug option enabled"
  debug "bc_cloner args: item=#{item}, bc=#{bc}, entity=#{entity}, source=#{source}, target=#{target}, replace=#{replace}"

  files = []
  new_item = (replace ? bc_replacer("#{item}", bc, entity) : item)
  new_file = File.join target, new_item
  new_source = File.join(source, item)
  debug "\tnew_source=#{new_source}"
  debug "\tnew_target=#{new_file} (item is #{new_item})"
  if File.directory? new_source
    debug "\tcreating directory #{new_file}."
    FileUtils.mkdir new_file unless File.directory? new_file
    clone = Dir.entries(new_source).find_all { |e| !e.start_with? '.'}
    clone.each do |recurse|
      files += bc_cloner(recurse, bc, entity, new_source, new_file, replace)
    end
  else
    #need to inject into the file (normal copy)
    unless replace
      debug "\t\tcopying file to #{new_file}."
      FileUtils.cp new_source, new_file
      files << new_file
    #we need to scrub the file for replacement strings
    else
      debug "\t\tcreating file #{new_file}."
      t = File.open(new_file, 'w')
      File.open(new_source, 'r') do |f|
        s = f.read
        t.write(bc_replacer(s, bc, entity))
      end
      t.close
      files << new_file
    end
  end
  return files
end

# this is used by the install-chef installer script 
if __FILE__ == $0
  bc = ARGV[0]
  org = ARGV[1] || "Dell, Inc."
  if bc.nil? or bc == ""
    puts "You must supply a name to create a barclamp"
    exit -3
  end
  path = ARGV[2] || "#{barclamp_dir}"
  target = File.join path, bc
  if File.exist? File.join target, "crowbar.yml"
    puts "Aborting! A barclamp already exists in '#{target}'."
    exit -3
  elsif ! ( bc =~ /^[a-zA-Z0-9_]*$/ )
    puts "You must supply a name to create a barclamp"
    exit -3
  else
    puts "Creating barclamp '#{bc}' into '#{path}' as entity '#{org}'."
    files = []
    FileUtils.mkdir target
    clone = Dir.entries(@MODEL_SOURCE).find_all { |e| !e.start_with? '.'}
    clone.each do |item|
      files += bc_cloner(item, bc, org, @MODEL_SOURCE, target, true)
    end
    filelist = "#{bc}-filelist.txt"
    File.open( filelist, 'w' ) do |out|
      files.each { |line| out.puts line }
    end
    puts "Barclamp #{bc} created in #{target}.  Review #{filelist} for files created."
  end
  exit 0
end
