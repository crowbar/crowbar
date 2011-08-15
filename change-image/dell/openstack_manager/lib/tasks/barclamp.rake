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

  BASE_PATH = '/opt/dell'
  BARCLAMP_PATH = File.join BASE_PATH, 'chef'
  CROWBAR_PATH = File.join BASE_PATH, 'openstack_manager'
  BIN_PATH = File.join BASE_PATH, 'bin'
  
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
        bc = s[/BARCLAMP_NAME=(.*)/,1] if bc.nil?
      end
      puts "Installing barclamp '#{bc}' from '#{path}'"
      
      # copy all the files to the target
      FileUtils.cp_r File.join(path, 'chef', '*'), BARCLAMP_PATH, :force
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
      FileUtils.cd File.join BARLCAMP_PATH, 'roles'
      roles.each do |role|
        knife_role = "knife role from file #{role}"
        system knife_role
        puts "\texecuted: #{knife_role}"
      end
      
      #copy the rails parts
      FileUtils.cp_r File.join(path, 'app', '*'), File.join(CROWBAR_PATH, 'app'), :force
      FileUtils.cp_r File.join(path, 'public', '*'), File.join(CROWBAR_PATH, 'public'), :force
      FileUtils.cp_r File,join(path, 'command_line', '*'), File.join(BIN_PATH), :force
      puts "\tcopied app & command line files"
      
      system "service apache2 reload"
      puts "\trestarted the web server"
      puts "done."
    end
  end

end