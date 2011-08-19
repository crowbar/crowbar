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

namespace :barclamp do

  MODEL_SOURCE = File.join 'lib', 'barclamp_model'
  MODEL_SUBSTRING_BASE = '==BC-MODEL=='
  MODEL_SUBSTRING_CAMEL = '==^BC-MODEL=='
  MODEL_TARGET = File.join '..', 'barclamps'
  BASE_PATH = File.join '/opt', 'dell'
  BARCLAMP_PATH = File.join BASE_PATH, 'chef'
  CROWBAR_PATH = File.join BASE_PATH, 'openstack_manager'
  BIN_PATH = File.join BASE_PATH, 'bin'
  
  desc "Create a new barclamp"
  task :create, [:name, :entity] do |t, args|
    args.with_defaults(:entity => 'Dell')
    bc = args.name
    if bc.nil?
      puts "You must supply a name to create a barclamp"
    else
      target = File.join MODEL_TARGET, bc
      puts "Creating barclamp '#{bc}' into '#{target}' as entity '#{args.entity}'."
      FileUtils.mkdir target
      clone = Dir.entries(MODEL_SOURCE).find_all { |e| !e.start_with? '.'}
      clone.each do |item|
        bc_cloner(item, bc, args.entity, MODEL_SOURCE, target)
      end
    end
  end
  
  def bc_cloner(item, bc, entity, source, target)
    new_item = bc_replacer(item, bc, entity)
    new_file = File.join target, new_item
    new_source = File.join(source, item)
    if File.directory? new_source
      #puts "\tcreating directory #{new_file}."
      FileUtils.mkdir new_file, :verbose => true
      clone = Dir.entries(new_source).find_all { |e| !e.start_with? '.'}
      clone.each do |recurse|
        bc_cloner(recurse, bc, entity, new_source, new_file)
      end
    else
      #need to inject into the file
      puts "\t\tcreating file #{new_file}."
      t = File.open(new_file, 'w')
      File.open(new_source, 'r') do |f|
        s = f.read
        t.write(bc_replacer(s, bc, entity))
      end
      t.close
    end
  end
  
  def bc_replacer(item, bc, entity)
    item = item.gsub(MODEL_SUBSTRING_BASE, bc)
    item = item.gsub(MODEL_SUBSTRING_CAMEL, bc.capitalize)
    item = item.gsub('Copyright 2011, Dell', "Copyright #{Time.now.year}, #{entity}")
    return item
  end
  
  desc "Install a barclamp into an active system"
  task :install, [:path] do |t, args|
    path = args.path || "."
    version = File.join path, 'version.sh'
    unless File.exist? version
      puts "could not install barclamp - failed to find #{version}"
    else
      bc = nil
      File.open(version, 'r') do |f|
        s = f.readline
        bc ||= s[/BARCLAMP_NAME=(.*)/,1].chomp.strip
      end
      puts "Installing barclamp #{bc} from #{path}"
      
      # copy all the files to the target
      FileUtils.cp_r File.join(path, 'chef'), BARCLAMP_PATH
      puts "\tcopied over chef parts from #{path} to #{BARCLAMP_PATH}"
      
      #upload the cookbooks
      FileUtils.cd File.join BARCLAMP_PATH, 'cookbooks'
      knife_cookbook = "knife cookbook upload -o . #{bc}"
      system knife_cookbook
      puts "\texecuted: #{knife_cookbook}"
      
      #upload the databags
      FileUtils.cd File.join BARCLAMP_PATH, 'data_bags', 'crowbar'
      knife_databag  = "knife data bag from file crowbar bc-template-#{bc}.json"
      system knife_databag
      puts "\texecuted: #{knife_databag}"

      #upload the roles
      roles = Dir.entries(File.join(path, 'chef', 'roles')).find_all { |r| r.end_with?(".rb") }
      FileUtils.cd File.join BARCLAMP_PATH, 'roles'
      roles.each do |role|
        knife_role = "knife role from file #{role}"
        system knife_role
        puts "\texecuted: #{knife_role}"
      end
      
      #copy the rails parts
      dirs = Dir.entries(path)
      FileUtils.cp_r File.join(path, 'app'), File.join(CROWBAR_PATH, 'app') if dirs.include?('app')
      FileUtils.cp_r File.join(path, 'public'), File.join(CROWBAR_PATH, 'public') if dirs.include? 'public'
      FileUtils.cp_r File.join(path, 'command_line'), File.join(BIN_PATH) if dirs.include? 'command_line'
      puts "\tcopied app & command line files"
      
      system "service apache2 reload"
      puts "\trestarted the web server"
      puts "done."
    end
  end

end
