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

  MODEL_SOURCE = File.join 'lib', 'barclamp_model'
  MODEL_SUBSTRING_BASE = '==BC-MODEL=='
  MODEL_SUBSTRING_CAMEL = '==^BC-MODEL=='
  MODEL_TARGET = File.join '..', 'barclamps'
  BASE_PATH = File.join '/opt', 'dell'
  BARCLAMP_PATH = File.join BASE_PATH, 'chef'
  CROWBAR_PATH = File.join BASE_PATH, 'crowbar_framework'
  BIN_PATH = File.join BASE_PATH, 'bin'
  UPDATE_PATH = '/updates'
  ROOT_PATH = '/'
  DEBUG=false
  
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
      nav_file = File.join 'config', 'navigation.rb'  #assume that we're in the app dir
      nav = []
      File.open(nav_file, 'r') do |f|
        nav << f.eachline { |line| nav.push line }
      end
      add = barclamp['nav']['add']
      # we only write the updated file if we are adding to it
      unless add.nil?
        File.open( nav_file, 'w') do |out|
          nav.each do |line|
            # remove the old menu item (works for install or uninstall)
            out.puts line unless line.lstrip.starts_with? "secondary.item :#{key}"
            # if you are installing, add the item under the barclamp menu.
            if installing and line.lstrip.starts_with? "primary.item :barclamps"
              add.each do |key, value|
                out.puts "secondary.item :#{key}, t('nav.#{key}'), #{value}" unless value.nil?
              end
            end
          end
        end
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
    filelist = File.join path, 'filelist.yml'
    master = YAML.load_file filelist
    master.each do |key, value|
      FileUtil.rm value
    end
    merge_nav barclamp, false
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
      puts "\tcopied over chef parts from #{path} to #{BARCLAMP_PATH}" if DEBUG
    end
  
    filelist = File.join path, 'filelist.yml'
    File.open( filelist, 'w' ) do |out|
      YAML.dump( {"files" => files }, out )
    end

    puts "Barclamp #{bc} (format v1) added to Crowbar Framework.  Review #{filelist} for files created." if DEBUG
  end
    
  def bc_install_layout_1_chef(bc, path, barclamp)

    #upload the cookbooks
    FileUtils.cd File.join path, 'chef', 'cookbooks'
    knife_cookbook = "knife cookbook upload -o . -a -k /etc/chef/webui.pem -u chef-webui"
    system knife_cookbook
    puts "\texecuted: #{path} #{knife_cookbook}" if DEBUG
    
    #upload the databags
    Dir.entries(File.join(path, 'chef', 'data_bags')).each do |bag|
      next if bag == "." or bag == ".."
      FileUtils.chmod 755, File.join(path, 'chef', 'data_bags', bag)
      chmod_dir 644, File.join(path, 'chef', 'data_bags', bag)
      FileUtils.cd File.join(path, 'chef', 'data_bags', bag)
      knife_bag  = "knife data bag create #{bag} -k /etc/chef/webui.pem -u chef-webui"
      system knife_bag
      puts "\texecuted: #{path} #{knife_bag}" if DEBUG

      json = Dir.entries(File.join(path, 'chef', 'data_bags', bag)).find_all { |r| r.end_with?(".json") }
      json.each do |bag_file|
        knife_databag  = "knife data bag from file #{bag} #{bag_file} -k /etc/chef/webui.pem -u chef-webui"
        system knife_databag
        puts "\texecuted: #{path} #{knife_databag}" if DEBUG
      end
    end

    #upload the roles
    roles = Dir.entries(File.join(path, 'chef', 'roles')).find_all { |r| r.end_with?(".rb") }
    FileUtils.cd File.join path, 'chef', 'roles'
    roles.each do |role|
      knife_role = "knife role from file #{role} -k /etc/chef/webui.pem -u chef-webui"
      system knife_role
      puts "\texecuted: #{path} #{knife_role}" if DEBUG
    end

    puts "Barclamp #{bc} (format v1) Chef Components Uploaded." if DEBUG

  end
  
  # this is used by the install-chef installer script becaues rake is not ready yet
  if __FILE__ == $0
    path = ARGV[0]
    puts "Using #{path}" if DEBUG
    barclamp = YAML.load_file File.join path, 'crowbar.yml'
    bc = barclamp["barclamp"]["name"].chomp.strip
    bc_install_layout_1_app bc, path, barclamp
    bc_install_layout_1_chef bc, path, barclamp
    #TODO add bag verify
    #err = verify_bags( base_dir)
    #exit -3 if err
    exit 0
  end
  
