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

require 'rubygems'
require 'yaml'
require 'json'
require 'fileutils'
require 'active_support/all'
require 'pp'

MODEL_SUBSTRING_BASE = '==BC-MODEL=='
MODEL_SUBSTRING_CAMEL = '==^BC-MODEL=='
MODEL_SUBSTRING_HUMAN = '==*BC-MODEL=='
MODEL_SUBSTRING_CAPSS = '==%BC-MODEL=='
if ENV["CROWBAR_DIR"]
  MODEL_SOURCE = File.join ENV["CROWBAR_DIR"], "barclamps","crowbar","crowbar_framework",'barclamp_model'
  BARCLAMP_PATH = File.join ENV["CROWBAR_DIR"], "barclamps"
else
  BASE_PATH = File.join '/opt', 'dell'
  BARCLAMP_PATH = File.join BASE_PATH, 'barclamps'
  CROWBAR_PATH = File.join BASE_PATH, 'crowbar_framework'
  MODEL_SOURCE = File.join CROWBAR_PATH, 'barclamp_model'
  BIN_PATH = File.join BASE_PATH, 'bin'
  UPDATE_PATH = '/updates'
  ROOT_PATH = '/'
end

@@debug = ENV['DEBUG'] === "true"

def debug(msg)
  puts "DEBUG: " + msg if @@debug
end

# entry point for scripts
def bc_install(bc, path, barclamp)
  case barclamp["crowbar"]["layout"].to_i
  when 1
    debug "Installing app components"
    bc_install_layout_1_app bc, path, barclamp
    debug "Installing chef components"
    bc_install_layout_1_chef bc, path, barclamp
    debug "Installing cache components"
    bc_install_layout_1_cache bc, path, barclamp
  else
    throw "ERROR: could not install barclamp #{bc} because #{barclamp["barclamp"]["crowbar_layout"]} is unknown layout."
  end
  catalog path
end

# regenerate the barclamp catalog (does a complete regen each install)
def catalog(path)
  debug "Creating catalog in #{path}"
  # create the groups for the catalog - for now, just groups.  other catalogs may be added later
  cat = { 'barclamps'=>{} }
  barclamps = File.join CROWBAR_PATH, 'barclamps'
  list = Dir.entries(barclamps).find_all { |e| e.end_with? '.yml'}
  # scan the installed barclamps
  list.each do |bc_file|
    debug "Loading #{bc_file}"
    bc = YAML.load_file File.join(barclamps, bc_file)
    name =  bc['barclamp']['name']
    cat['barclamps'][name] = {} if cat['barclamps'][name].nil?
    description = bc['barclamp']['description']
    if description.nil?
      debug "Trying to find description"
      [ File.join(path, '..', name, 'chef', 'data_bags', 'crowbar', "bc-template-#{name}.json"), \
        File.join(path, '..', "barclamp-#{name}", 'chef', 'data_bags', 'crowbar', "bc-template-#{name}.json")].each do |f|
        next unless File.exist? f
        s = JSON::load File.open(f, 'r')
        description = s['description'] unless s.nil?
	break if description
      end
    end
    # template = File.join path, name,
    debug "Adding catalog info for #{bc['barclamp']['name']}"
    cat['barclamps'][name]['description'] = description || "No description for #{bc['barclamp']['name']}"
    cat['barclamps'][name]['user_managed'] = (bc['barclamp']['user_managed'].nil? ? true : bc['barclamp']['user_managed'])
    puts "#{name} #{bc['barclamp']['user_managed']}" if name === 'dell-branding'
    bc['barclamp']['member'].each do |meta|
      cat['barclamps'][meta] = {} if cat['barclamps'][meta].nil?
      cat['barclamps'][meta]['members'] = {} if cat['barclamps'][meta]['members'].nil?
      cat['barclamps'][meta]['members'][name] = bc['crowbar']['order']
    end if bc['barclamp']['member']

    cat['barclamps'][name]['order'] = bc['crowbar']['order'] if bc['crowbar']['order']
    cat['barclamps'][name]['run_order'] = bc['crowbar']['run_order'] if bc['crowbar']['run_order']
    cat['barclamps'][name]['chef_order'] = bc['crowbar']['chef_order'] if bc['crowbar']['chef_order']
    if bc['git']
      cat['barclamps'][name]['date'] = bc['git']['date'] if bc['git']['date']
      cat['barclamps'][name]['commit'] = bc['git']['commit'] if bc['git']['commit']
    end

  end
  File.open( File.join(CROWBAR_PATH, 'config', 'catalog.yml'), 'w' ) do |out|
    YAML.dump( cat, out )
  end
end

# copies paths from one place to another (recursive)
def bc_cloner(item, bc, entity, source, target, replace)
  debug "bc_cloner method called with debug option enabled"
  debug "bc_cloner args: item=#{item}, bc=#{bc}, entity=#{entity}, source=#{source}, target=#{target}, replace=#{replace}"

  files = []
  new_item = (replace ? bc_replacer(item, bc, entity) : item)
  debug "new_item=#{new_item}"
  new_file = File.join target, new_item
  debug "new_file=#{new_file}"
  new_source = File.join(source, item)
  debug "new_source=#{new_source}"
  if File.directory? new_source
    debug "\tcreating directory #{new_file}."
    FileUtils.mkdir new_file unless File.directory? new_file
    clone = Dir.entries(new_source).find_all { |e| !e.start_with? '.'}
    clone.each do |recurse|
      files += bc_cloner(recurse, bc, entity, new_source, new_file, replace)
    end
  else
    #need to inject into the file
    unless replace
      debug "\t\tcopying file #{new_file}."
      FileUtils.cp new_source, new_file
      files << new_file
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

# fix permissions
def chmod_dir(value, path)
  f = Dir.entries(path).find_all { |e| !e.start_with? '.'}
  f.each do |i|
    file = File.join(path,i)
    if File.exists? file
      FileUtils.chmod value, file
      debug "\tchmod 0#{value.to_s(8)} for #{file}"
    else
      puts "WARN: missing file #{file} for chmod #{value} operation."
    end
  end
end

# remove model placeholders
def bc_replacer(item, bc, entity)
  debug "bc_replacer method called with debug option enabled"
  debug "bc_replacer args: item=#{item}, bc=#{bc}, entity=#{entity}"

  item.gsub!(MODEL_SUBSTRING_BASE, bc)
  item.gsub!(MODEL_SUBSTRING_CAMEL, bc.camelize)
  item.gsub!(MODEL_SUBSTRING_HUMAN, bc.humanize)
  item.gsub!(MODEL_SUBSTRING_CAPSS, bc.capitalize)
  item.gsub!('Copyright 2011, Dell', "Copyright #{Time.now.year}, #{entity}")
  debug "bc_replacer returns item=#{item}"
  return item
end

#merges localizations from config into the matching translation files
def merge_i18n(barclamp)
  locales = barclamp['locale_additions']
  locales.each do |key, value|
    #translation file (can be multiple)
    f = File.join CROWBAR_PATH, 'config', 'locales', "#{key}.yml"
    if File.exist? f
      debug "merging translation for #{f}"
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

# makes sure that sass overrides are injected into the application.sass
def merge_sass(barclamp, bc, path, installing)
  sass_path = File.join path, 'crowbar_framework', 'public', 'stylesheets', 'sass'
  application_sass = File.join CROWBAR_PATH, 'public', 'stylesheets', 'sass', 'application.sass'
  if File.exist? application_sass and File.exists? sass_path
    sass_files = Dir.entries(sass_path).find_all { |r| r =~ /^_(.*).sass$/ }
    # get entries from the applicaiton.sass file
    sapp = []
    File.open(application_sass,'r') do |f|
      f.each_line { |l| sapp << l.chomp }
    end
    # figure out where to insert the sass item
    top = -1
    if !barclamp['application_sass'].nil? and  barclamp['application_sass']['add'] === 'top'
      top = (sapp.find_index("// top of import list") || 3)+1
    end
    # remove items that we don't want
    barclamp['application_sass']['remove'].each do |item|
      if installing and sapp.include? item
        sapp.delete item
        debug "removing '#{item}' from application.sass based on crowbar.yml"
      elsif !installing and !sapp.include? item
        if top>0
          sapp.insert top, item
        else
          sapp << item
        end
        debug "restoring '#{item}' to application.sass based on crowbar.yml in position #{top}"
      end
    end unless barclamp['application_sass'].nil? or barclamp['application_sass']['remove'].nil?
    # scan the sass files from the barclamp
    sass_files.each do |sf|
      entry = "@import #{sf[/^_(.*).sass$/,1]}"
      # when installing, if not already in the application, add it
      if installing and !sapp.include? entry
        if top>0
          sapp.insert top, entry
        else
          sapp << entry
        end
        debug "adding '#{entry}' to application.sass for #{sf} in position #{top}"
        # when uninstalling, remove from applicaiton
      elsif !installing
        sapp.delete entry
        debug "removing '#{entry}' from application.sass for #{sf}"
      end
    end
    # write the new application sass
    File.open(application_sass, 'w' ) do |out|
      out.puts sapp
    end
    framework_permissions bc, path

    debug "updated #{application_sass}"
  else
    debug "NOTE: skipping application sass update, #{application_sass} not found"
  end
end

# injects/cleans barclamp items from framework navigation
def merge_nav(barclamp, installing)
  unless barclamp['nav'].nil?
    bc_flag = "#FROM BARCLAMP: #{barclamp['barclamp']['name']}."
    # get raw file
    nav_file = File.join CROWBAR_PATH, 'config', 'navigation.rb'
    nav_raw = []
    File.open(nav_file, 'r') do |f|
      f.each_line { |line| nav_raw << line }
    end
    # remove stuff that will be replaced
    nav = []
    nav_raw.each do |line|
      nav << line unless line =~ /#{bc_flag}$/
    end
    # now add new items
    new_nav = []
    nav.each do |line|
      unless barclamp['nav']['primary'].nil?
        barclamp['nav']['primary'].each do |key, value|
          #insert new items before
          new_nav << "primary.item :#{value} #{bc_flag}" if installing and line.lstrip.start_with? "primary.item :#{key}"
        end
      end
      # add the line
      new_nav << line
      # add submenu items (REQUIRIES KEYS IN NAV FILE!!)
      barclamp['nav'].each do |key, value|
        if installing and line.lstrip.start_with? "# insert here for :#{key}"
          value.each do |k, v|
            new_nav << "secondary.item :#{k}, t('nav.#{k}'), #{v} #{bc_flag}" unless v.nil?
          end
        end
      end
    end
    File.open( nav_file, 'w') do |out|
      new_nav.each { |l| out.puts l }
    end
  end
end

# helper for localization merge
def merge_tree(key, value, target)
  if target.key? key
    if target[key].class == Hash
      value.each do |k, v|
        #puts "recursing into tree at #{key} for #{k}"
        target[key] = merge_tree(k, v, target[key])
      end
    else
      debug "replaced key #{key} value #{value}"
      target[key] = value
    end
  else
    debug "added key #{key} value #{value}"
    target[key] = value
  end
  return target
end

# cleanup (anti-install) assumes the install generates a file list
def bc_remove_layout_1(bc, path, barclamp)
  filelist = File.join BARCLAMP_PATH, "#{bc}-filelist.txt"
  if File.exist? filelist
    files = [ filelist ]
    File.open(filelist, 'r') do |f|
      f.each_line { |line| files << line }
    end
    FileUtils.rm files
    merge_nav barclamp, false
    merge_sass barclamp, bc, path, false
    debug "Barclamp #{bc} UNinstalled"
  end
end

def framework_permissions(bc, path)
  FileUtils.chmod 0755, File.join(CROWBAR_PATH, 'db')
  chmod_dir 0644, File.join(CROWBAR_PATH, 'db')
  FileUtils.chmod 0755, File.join(CROWBAR_PATH, 'tmp')
  chmod_dir 0644, File.join(CROWBAR_PATH, 'tmp')
  FileUtils.chmod_R 0755, File.join(CROWBAR_PATH, 'public', 'stylesheets')
  debug "\tcopied crowbar_framework files"
end

# install the framework files for a barclamp
def bc_install_layout_1_app(bc, path, barclamp)

  #TODO - add a roll back so there are NOT partial results if a step fails
  files = []

  puts "Installing barclamp #{bc} from #{path}"

  #copy the rails parts (required for render BEFORE import into chef)
  dirs = Dir.entries(path)
  debug "path entries #{dirs.pretty_inspect}"
  if dirs.include? 'crowbar_framework'
    debug "path entries include \"crowbar_framework\""
    files += bc_cloner('crowbar_framework', bc, nil, path, BASE_PATH, false)
    framework_permissions bc, path
  end

  #merge i18n information (least invasive operations first)
  debug "merge_i18n"
  merge_i18n barclamp
  debug "merge_nav"
  merge_nav barclamp, true
  debug "merge_sass"
  merge_sass barclamp, bc, path, true

  if dirs.include? 'bin'
    debug "path entries include \"bin\""
    files += bc_cloner('bin', bc, nil, path, BASE_PATH, false)
    FileUtils.chmod_R 0755, BIN_PATH
    debug "\tcopied command line files"
  end
  if dirs.include? 'updates'
    debug "path entries include \"updates\""
    files += bc_cloner('updates', bc, nil, path, ROOT_PATH, false)
    FileUtils.chmod_R 0755, UPDATE_PATH
    debug "\tcopied updates files"
  end

  # copy all the files to the target
  if dirs.include? 'chef'
    debug "path entries include \"chef\""
    files += bc_cloner('chef', bc, nil, path, BASE_PATH, false)
    debug "\tcopied over chef parts from #{path} to #{BASE_PATH}"
  end

  # Migrate base crowbar schema if needed
  bc_schema_version = barclamp["crowbar"]["proposal_schema_version"].to_i rescue 1
  if bc_schema_version < 2
    name = barclamp['barclamp']['name']
    schema_file = File.join BASE_PATH, 'chef','data_bags','crowbar', "bc-template-#{name}.schema"
    if File.exists? schema_file
      a = []
      File.open(schema_file, 'r') { |f|
        a = f.readlines
      }

      File.open(schema_file, 'w') { |f|
        a.each do |line|
          if line =~ /crowbar-committing/
            f.write("  \"crowbar-status\": { \"type\": \"str\" },\n")
            f.write("  \"crowbar-failed\": { \"type\": \"str\" },\n")
          end
          f.write(line)
        end
      }
    end
  end

  filelist = File.join BARCLAMP_PATH, "#{bc}-filelist.txt"
  File.open( filelist, 'w' ) do |out|
    files.each { |line| out.puts line }
  end

  #copy over the crowbar.yml file
  yml_path = File.join CROWBAR_PATH, 'barclamps'
  yml_barclamp = File.join path, "crowbar.yml"
  FileUtils.mkdir yml_path unless File.directory? yml_path
  FileUtils.cp yml_barclamp, File.join(yml_path, "#{bc}.yml")

  debug "Barclamp #{bc} (format v1) added to Crowbar Framework.  Review #{filelist} for files created."
end

# upload the chef parts for a barclamp
def bc_install_layout_1_chef(bc, path, barclamp)

  log_path = File.join '/var', 'log', 'barclamps'
  FileUtils.mkdir log_path unless File.directory? log_path
  log = File.join log_path, "#{bc}.log"
  system "date >> #{log}"
  debug "Capturing chef install logs to #{log}"
  chef = File.join path, 'chef'
  cookbooks = File.join chef, 'cookbooks'
  databags = File.join chef, 'data_bags'
  roles = File.join chef, 'roles'

  #upload the cookbooks
  if File.directory? cookbooks
    FileUtils.cd cookbooks
    knife_cookbook = "knife cookbook upload -o . -a -V -k /etc/chef/webui.pem -u chef-webui"
    unless system knife_cookbook + " >> #{log} 2>&1"
      puts "\t#{path} #{knife_cookbook} upload failed. Examine #{log} for more into"
      exit 1
    end
    debug "\texecuted: #{path} #{knife_cookbook}"
  else
    debug "\tNOTE: could not find cookbooks #{cookbooks}"
  end

  #upload the databags
  if File.exists? databags
    Dir.entries(databags).each do |bag|
      next if bag == "." or bag == ".."
      bag_path = File.join databags, bag
      FileUtils.chmod 0755, bag_path
      chmod_dir 0644, bag_path
      FileUtils.cd bag_path
      knife_bag  = "knife data bag create #{bag} -V -k /etc/chef/webui.pem -u chef-webui"
      unless system knife_bag + " >> #{log} 2>&1"
        puts "\t#{knife_bag} failed.  Examine #{log} for more information."
        exit 1
      end
      debug "\texecuted: #{path} #{knife_bag}"

      json = Dir.entries(bag_path).find_all { |r| r.end_with?(".json") }
      json.each do |bag_file|
        knife_databag  = "knife data bag from file #{bag} #{bag_file} -V -k /etc/chef/webui.pem -u chef-webui"
          unless system knife_databag + " >> #{log} 2>&1"
            puts "\t#{knife_databag} failed.  Examine #{log} for more information."
            exit 1
          end
        debug "\texecuted: #{path} #{knife_databag}"
      end
    end
  else
    debug "\tNOTE: could not find databags #{databags}"
  end

  #upload the roles
  if File.directory? roles
    FileUtils.cd roles
    Dir.entries(roles).find_all { |r| r.end_with?(".rb") }.each do |role|
      knife_role = "knife role from file #{role} -V -k /etc/chef/webui.pem -u chef-webui"
      unless system knife_role + " >> #{log} 2>&1"
        puts "\t#{knife_role} failed.  Examine #{log} for more information."
        exit 1
      end
      debug "\texecuted: #{path} #{knife_role}"
    end
  else
    debug "\tNOTE: could not find roles #{roles}"
  end

  puts "Barclamp #{bc} (format v1) Chef Components Uploaded."

end

def bc_install_layout_1_cache(bc, path, barclamp)
  return unless File.directory?(File.join(path,"cache"))
  Dir.entries(File.join(path,"cache")).each do |ent|
    debug ent.inspect
    case
    when ent == "files"
      debug "Copying files"
      system "cp -r \"#{path}/cache/#{ent}\" /tftpboot"
    when ent == "gems"
      # Symlink the gems into One Flat Directory.
      debug "Installing gems"
      Dir.entries("#{path}/cache/gems").each do |gem|
        next unless /\.gem$/ =~ gem
        unless File.directory? "/tftpboot/gemsite/gems"
          system "mkdir -p /tftpboot/gemsite/gems"
        end
        unless File.symlink? "/tftpboot/gemsite/gems/#{gem}"
	  debug "Symlinking #{path}/cache/gems/#{gem} into /tftpboot/gemsite/gems"
          File.symlink "#{path}/cache/gems/#{gem}", "/tftpboot/gemsite/gems/#{gem}"
        end
      end
      debug "Done"
    when File.directory?("#{path}/cache/#{ent}/pkgs")
      debug "Installing packages"
      # We have actual packages here.  They map into the target like so:
      # path/ent/pkgs -> /tftboot/ent/crowbar-extras/bc
      unless File.directory? "/tftpboot/#{ent}/crowbar-extra/"
        system "mkdir -p \"/tftpboot/#{ent}/crowbar-extra/\""
      end
      # sigh, ubuntu-install and redhat-install.
      unless File.symlink? "/tftpboot/#{ent}/crowbar-extra/#{path.split('/')[-1]}"
	debug "Symlinking #{path}/cache/#{ent}/pkgs into /tftpboot/#{ent}/crowbar-extra"
        File.symlink "#{path}/cache/#{ent}/pkgs", "/tftpboot/#{ent}/crowbar-extra/#{path.split('/')[-1]}"
      end
    end
    debug "Done"
    true
  end
end
