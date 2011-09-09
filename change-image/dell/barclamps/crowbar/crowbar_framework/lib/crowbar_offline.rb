# Copyright 2011, Dell 
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
# Author: RobHirschfeld 
# 
module CrowbarOffline
#/lib/crowbar_offline.rb
  
    OFFLINE_FILES_DIR = 'db'
    
    def nfile(serial, name)
      n = if name.nil?
        "#{serial}.index"
      elsif serial.nil?
        "#{name}.json"
      else
        "#{serial}-#{name}.json"
      end
      File.join(OFFLINE_FILES_DIR, n)
    end
    
    # returns list of files that match the criteria
    def offline_search(serial, search)
      files = Dir.entries(OFFLINE_FILES_DIR).find_all { |f| /^#{serial}#{search}(.*).json$/ =~ f }
      files.map! { |f| File.join(OFFLINE_FILES_DIR, f) } 
      tfiles = Dir.entries("tmp").find_all { |f| /^#{serial}#{search}(.*).json$/ =~ f }
      tfiles.map! { |f| File.join(OFFLINE_FILES_DIR, f) }
      (tfiles+files).uniq
    end
    
    def dump(o, serial, name)
      file = nfile(serial, name)
      unless File.exist? file
        begin 
          File.open(file, 'w') do |f|
            f.puts o.to_json
          end
          Rails.logger.info("Dumped Chef #{serial} #{name} object to '#{file}'")
        rescue Exception => e
          Rails.logger.warn("Error dumping Chef #{serial} object to '#{name}' with '#{e.inspect}'")
        end #dump object
        return true
      else
        return false
      end #unless exist
    end
    
    def offline_cache(o, file)
      cache = File.join('tmp', file[/#{OFFLINE_FILES_DIR}[\/\\](.*)/,1])
      File.open(cache, 'w') {  |f| f.puts o.to_json }
      Rails.logger.debug "Cached #{o} to #{cache}"
    end
    
    def clear_cache(o)
      file = case o.class.to_s
      when 'Chef::Node'
        "node-#{o.name}.json"
      when 'Chef::Role'
        "role-#{o.name}.json"
      when 'Chef::DataBagItem'
        "data-bag-item_#{o.name}.json"
      else
        throw "Crowbar Offline Clear Cache missing class response for '#{o.class.to_s}'"
      end
      file = File.join('tmp', file)
      Rails.logger.info("Removed Cached Object '#{file}'")
      puts "Cache delete #{file}"
      File.delete(file) if File.exists? file
    end
    
    def create_object(type, name, description)
      template =  File.join('lib', 'offline', "#{type}-.template")
      file = File.join(OFFLINE_FILES_DIR, "#{type}-#{name}.json")
      if File.exist? file
        throw "Cannot create new objects because one exists at '#{file}'."
      end
      if File.exist? template
        base = File.open(template, 'r').gets
        base = base.gsub("$NAME$", name)
        base = base.gsub("$DESCRIPTION$", description || "Default offline object for #{name}")
        base = base.gsub("$GUID$", "0-OfflineModeHasNoGUIDs")
        File.open(file, 'w') do |f|
          f.puts base
        end
      else
        throw "No Offline Template found matching '#{template}'."
      end
    end
    
    def recover_json(file)
      cache = File.join('tmp', file[/#{OFFLINE_FILES_DIR}[\/\\](.*)/,1])
      if File.exists? cache
        Rails.logger.debug("Cache hit for #{cache}") 
        file = cache
      end
      begin
        if File.exist? file
          return JSON::load File.open("#{file}", 'r')
        else
          Rails.logger.warn("Error file not found for chef object.  Looking for '#{file}'")
        end
      rescue Exception => e          
        Rails.logger.error("Error recovering chef object from '#{file}' caused by error '#{e.inspect}'")
      end
      return nil
    end
    
end
