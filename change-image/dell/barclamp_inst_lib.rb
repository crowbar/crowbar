#!/usr/bin/ruby
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

  require 'yaml'
  require 'fileutils'

  MODEL_SOURCE = File.join '/opt', 'dell', 'barclamp_model'
  MODEL_SUBSTRING_BASE = '==BC-MODEL=='
  MODEL_SUBSTRING_CAMEL = '==^BC-MODEL=='
  BASE_PATH = File.join '/opt', 'dell'
  MODEL_TARGET = File.join BASE_PATH, 'barclamps'
  BARCLAMP_PATH = File.join BASE_PATH, 'barclamps'
  CROWBAR_PATH = File.join BASE_PATH, 'crowbar_framework'
  BIN_PATH = File.join BASE_PATH, 'bin'
  UPDATE_PATH = '/updates'
  ROOT_PATH = '/'
  DEBUG=true
  
  def bc_cloner(item, bc, entity, source, target, replace)
    files = []
    new_item = (replace ? bc_replacer(item, bc, entity) : item)
    new_file = File.join target, new_item
    new_source = File.join(source, item)
    if File.directory? new_source
      puts "\tcreating directory #{new_file}." if DEBUG
      FileUtils.mkdir new_file unless File.directory? new_file
      clone = Dir.entries(new_source).find_all { |e| !e.start_with? '.'}
      clone.each do |recurse|
        files += bc_cloner(recurse, bc, entity, new_source, new_file, replace)
      end
    else
      #need to inject into the file
      unless replace
        puts "\t\tcopying file #{new_file}." if DEBUG
        FileUtils.cp new_source, new_file
        files << new_file
      else
        puts "\t\tcreating file #{new_file}." if DEBUG
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
  
  def chmod_dir(value, path)
    f = Dir.entries(path).find_all { |e| !e.start_with? '.'}
    f.each do |i|
      file = File.join(path,i)
      if File.exists? file
        FileUtils.chmod value, file
        puts "\tchmod #{value} for #{file}" if DEBUG
      else
        puts "WARN: missing file #{file} for chmod #{value} operation."
      end
    end
  end
  
  def bc_replacer(item, bc, entity)
    item = item.gsub(MODEL_SUBSTRING_BASE, bc)
    item = item.gsub(MODEL_SUBSTRING_CAMEL, bc.capitalize)
    item = item.gsub('Copyright 2011, Dell', "Copyright #{Time.now.year}, #{entity}")
    return item
  end

  #merges localizations from config into the matching translation files
  def merge_i18n(barclamp)
    locales = barclamp['locale_additions']
    locales.each do |key, value|
      #translation file (can be multiple)
      f = File.join CROWBAR_PATH, 'config', 'locales', "#{key}.yml"
      if File.exist? f
        puts "merging tranlation for #{f}" if DEBUG
        master = YAML.load_file f
        master = merge_tree(key, value, master)
        File.open( f, 'w' ) do |out|
          YAML.dump( master, out )
        end
      else
        puts "WARNING: Did not attempt tranlation merge for #{f} because file was not found."
      end
    end
  end
  
  def merge_nav(barclamp, installing)
    unless barclamp['nav'].nil?
      # get raw file
      nav_file = File.join CROWBAR_PATH, 'config', 'navigation.rb'  
      nav_raw = []
      File.open(nav_file, 'r') do |f|
        f.each_line { |line| nav_raw << line }
      end
      # remove stuff that may be replaced
      nav = []
      nav_raw.each do |line|
        barclamp['nav']['primary'].each { |key, value| line = nil if line.lstrip.start_with? "primary.item :#{value}" } unless barclamp['nav']['primary'].nil?
        barclamp['nav']['add'].each { |key, value| line = nil if line.lstrip.start_with? "secondary.item :#{key}" } unless barclamp['nav']['add'].nil?
        nav << line unless line.nil?
      end  
      # now add new items
      new_nav = []
      nav.each do |line|
        unless barclamp['nav']['primary'].nil?
          barclamp['nav']['primary'].each do |key, value|
            #insert new items before
            new_nav << "primary.item :#{value}" if installing and line.lstrip.start_with? "primary.item :#{key}" 
          end
        end
        # add the line
        new_nav << line
        # add subitems under barclamps
        if installing and line.lstrip.start_with? "primary.item :barclamps" and !barclamp['nav']['add'].nil?
          barclamp['nav']['add'].each do |key, value|
            new_nav << "secondary.item :#{key}, t('nav.#{key}'), #{value}" unless value.nil?
          end
        end
      end
      File.open( nav_file, 'w') do |out|
        new_nav.each { |l| out.puts l }
      end
    end
  end
  
  def merge_tree(key, value, target)
    if target.key? key
      if target[key].class == Hash
        value.each do |k, v|
          #puts "recursing into tree at #{key} for #{k}"
          target[key] = merge_tree(k, v, target[key])
        end
      else
        #puts "replaced key #{key} value #{value}"
        target[key] = value      
      end
    else
      #puts "added key #{key} value #{value}"
      target[key] = value
    end
    return target
  end
  
  def bc_remove_layout_1(bc, path, barclamp)
    filelist = File.join BARCLAMP_PATH, "#{bc}-filelist.txt"
    if File.exist? filelist      
      files = [ filelist ]
      File.open(filelist, 'r') do |f|
        f.each_line { |line| files << line }
      end
      FileUtils.rm files
      merge_nav barclamp, false
      puts "Barclamp #{bc} UNinstalled" if DEBUG
    end
  end

  def bc_install_layout_1_app(bc, path, barclamp)
    
    #TODO - add a roll back so there are NOT partial results if a step fails
    files = []
    
    puts "Installing barclamp #{bc} from #{path}"

    #merge i18n information (least invasive operations first)
    merge_i18n barclamp
    merge_nav barclamp, true

    #copy the rails parts (required for render BEFORE import into chef)
    dirs = Dir.entries(path)
    if dirs.include? 'crowbar_framework'
      files += bc_cloner('crowbar_framework', bc, nil, path, BASE_PATH, false) 
      FileUtils.chmod 755, File.join(CROWBAR_PATH, 'db')
      chmod_dir 644, File.join(CROWBAR_PATH, 'db')
      FileUtils.chmod 755, File.join(CROWBAR_PATH, 'tmp')
      chmod_dir 766, File.join(CROWBAR_PATH, 'tmp')
      FileUtils.chmod_R 755, File.join(CROWBAR_PATH, 'public', 'stylesheets')
      puts "\tcopied crowbar_framework files" if DEBUG
    end
    if dirs.include? 'bin'
      files += bc_cloner('bin', bc, nil, path, BASE_PATH, false) 
      FileUtils.chmod_R 777, BIN_PATH
      puts "\tcopied command line files" if DEBUG
    end
    if dirs.include? 'updates'
      files += bc_cloner('updates', bc, nil, path, ROOT_PATH, false)
      puts "\tcopied updates files" if DEBUG
    end
    
    # copy all the files to the target
    if dirs.include? 'chef'
      files += bc_cloner('chef', bc, nil, path, BASE_PATH, false)
      puts "\tcopied over chef parts from #{path} to #{BASE_PATH}" if DEBUG
    end
  
    filelist = File.join BARCLAMP_PATH, "#{bc}-filelist.txt"
puts "FILES #{files.inspect}"
    File.open( filelist, 'w' ) do |out|
      files.each { |line| out.puts line } 
    end

    puts "Barclamp #{bc} (format v1) added to Crowbar Framework.  Review #{filelist} for files created." if DEBUG
  end
    
  def bc_install_layout_1_chef(bc, path, barclamp)

    chef = File.join path, 'chef'
    cookbooks = File.join chef, 'cookbooks'
    databags = File.join chef, 'data_bags'
    roles = File.join chef, 'roles'

    #upload the cookbooks
    if File.directory? cookbooks
      knife_cookbook = "knife cookbook upload -o . -a -k /etc/chef/webui.pem -u chef-webui"
      system knife_cookbook
      puts "\texecuted: #{path} #{knife_cookbook}" if DEBUG
    end
    
    #upload the databags
    Dir.entries(databags).each do |bag|
      next if bag == "." or bag == ".."
      bag_path = File.join databags, bag 
      FileUtils.chmod 755, bag_path
      chmod_dir 644, bag_path
      FileUtils.cd bag_path
      knife_bag  = "knife data bag create #{bag} -k /etc/chef/webui.pem -u chef-webui"
      system knife_bag
      puts "\texecuted: #{path} #{knife_bag}" if DEBUG

      json = Dir.entries(bag_path).find_all { |r| r.end_with?(".json") }
      json.each do |bag_file|
        knife_databag  = "knife data bag from file #{bag} #{bag_file} -k /etc/chef/webui.pem -u chef-webui"
        system knife_databag
        puts "\texecuted: #{path} #{knife_databag}" if DEBUG
      end
    end

    #upload the roles
    FileUtils.cd roles
    Dir.entries(roles).find_all { |r| r.end_with?(".rb") }.each do |role|
      knife_role = "knife role from file #{role} -k /etc/chef/webui.pem -u chef-webui"
      system knife_role
      puts "\texecuted: #{path} #{knife_role}" if DEBUG
    end

    puts "Barclamp #{bc} (format v1) Chef Components Uploaded." if DEBUG

  end
  