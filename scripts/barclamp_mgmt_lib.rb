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

require 'rubygems'
require 'fileutils'
require 'yaml'
require 'json'
require 'time'
require 'tempfile'
require 'active_support/all'
require 'pp'
require 'i18n'
require 'pathname'

if I18n.respond_to? :enforce_available_locales
  I18n.enforce_available_locales = false
end

MODEL_SUBSTRING_BASE = '==BC-MODEL=='
MODEL_SUBSTRING_CAMEL = '==^BC-MODEL=='
MODEL_SUBSTRING_HUMAN = '==*BC-MODEL=='
MODEL_SUBSTRING_CAPSS = '==%BC-MODEL=='

if ENV["CROWBAR_DIR"]
  BASE_PATH = ENV["CROWBAR_DIR"]
  BARCLAMP_PATH = File.join BASE_PATH, 'barclamps'
  CROWBAR_PATH = File.join BASE_PATH, 'crowbar_framework'
  MODEL_SOURCE = File.join CROWBAR_PATH, 'barclamp_model'
  BIN_PATH = File.join BASE_PATH, 'bin'
  UPDATE_PATH = '/updates'
  ROOT_PATH = '/'
else
  BASE_PATH = File.join '/opt', 'dell'
  BARCLAMP_PATH = File.join BASE_PATH, 'barclamps'
  CROWBAR_PATH = File.join BASE_PATH, 'crowbar_framework'
  MODEL_SOURCE = File.join CROWBAR_PATH, 'barclamp_model'
  BIN_PATH = File.join BASE_PATH, 'bin'
  UPDATE_PATH = '/updates'
  ROOT_PATH = '/'
end

def debug(msg)
  puts "DEBUG: " + msg if ENV['DEBUG'] === "true"
end

def fatal(msg, log = nil, exit_code = 1)
  str  = "ERROR: #{msg}  Aborting."
  if log
    str += " Examine #{log} for more info."
  end
  puts str
  exit exit_code
end

def get_crowbar_yml_path(directory, suggested_bc_name = nil)
  if suggested_bc_name.nil?
    suggested_bc_name = directory.split(File::SEPARATOR)[-1]
  end

  # Look for both old name (crowbar.yml) and new name ($barclamp.yml)
  ["#{suggested_bc_name}.yml", "crowbar.yml"].each do |basename|
    path = File.join(directory, basename)
    return path if File.exists?(path)
  end

  nil
end

# entry point for scripts
def bc_install(from_rpm, bc, bc_path, yaml)
  case yaml["crowbar"]["layout"].to_i
  when 1
    debug "Installing app components"
    bc_install_layout_1_app from_rpm, bc, bc_path, yaml
    debug "Installing chef components"
    bc_install_layout_1_chef from_rpm, bc, bc_path, yaml
    debug "Installing cache components"
    bc_install_layout_1_cache from_rpm, bc, bc_path, yaml
  else
    raise "ERROR: could not install barclamp #{bc} because #{yaml["barclamp"]["crowbar_layout"]} is unknown layout."
  end
  generate_navigation
  generate_assets_manifest
  catalog bc_path
end

# regenerate the barclamp catalog (does a complete regen each install)
def catalog(bc_path)
  debug "Creating catalog in #{bc_path}"
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
    display = bc['barclamp']['display']
    if description.nil?
      debug "Trying to find description"
      [ File.join(bc_path, '..', name, 'chef', 'data_bags', 'crowbar', "bc-template-#{name}.json"), \
        File.join(bc_path, '..', "barclamp-#{name}", 'chef', 'data_bags', 'crowbar', "bc-template-#{name}.json"), \
        File.join(bc_path, '..', '..', 'chef', 'data_bags', 'crowbar', "bc-template-#{name}.json") ].each do |f|
        next unless File.exist? f
        s = JSON::load File.open(f, 'r')
        description = s['description'] unless s.nil?
        break if description
      end
    end
    # template = File.join bc_path, name,
    debug "Adding catalog info for #{bc['barclamp']['name']}"
    cat['barclamps'][name]['description'] = description || "No description for #{bc['barclamp']['name']}"
    cat['barclamps'][name]['display'] = display || ""
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
    # git tagging
    cat['barclamps'][name]['date'] = I18n.t('unknown')
    cat['barclamps'][name]['commit'] = I18n.t('not_set')
    if bc['git']
      cat['barclamps'][name]['date'] = bc['git']['date'] if bc['git']['date']
      cat['barclamps'][name]['commit'] = bc['git']['commit'] if bc['git']['commit']
    end

  end
  File.open( File.join(CROWBAR_PATH, 'config', 'catalog.yml'), 'w' ) do |out|
    YAML.dump( cat, out )
  end
end

def stringify_options(hash)
  hash.map do |key, value|
    case
    when key == :if
      "#{key}: proc { #{value} }"
    when key == :unless
      "#{key}: proc { #{value} }"
    else
      if value.is_a? Hash
        "#{key}: { #{stringify_options(value)} }"
      else
        "#{key}: #{value.inspect}"
      end
    end
  end.join(", ")
end

def prepare_navigation(hash, breadcrumb, indent, level)
  temp = hash.sort_by do |key, values|
    (values["order"] || 1000).to_i
  end

  [].tap do |result|
    ActiveSupport::OrderedHash[temp].each do |key, values|
      current_path = breadcrumb.dup.push key

      order = values.delete("order")
      url = values.delete("url")
      route = values.delete("route")
      params = values.delete("params")
      path = values.delete("path")
      html = values.delete("html")
      options = values.delete("options") || {}

      options.symbolize_keys!

      link = case
      when route
        if params
          "#{route}(#{stringify_options(params)})"
        else
          route
        end
      when path
        if html
          options[:link] = html
        end

        path.inspect
      when url
        url.inspect
      end

      options_string = stringify_options(options)
      options_string.prepend(", ") unless options_string.empty?

      if values.keys.empty?
        result.push "level#{level}.item :#{key}, t(\"nav.#{current_path.join(".")}\"), #{link}#{options_string}".indent(indent)
      else
        result.push "level#{level}.item :#{key}, t(\"nav.#{current_path.join(".")}.title\"), #{link}#{options_string} do |level#{level + 1}|".indent(indent)
        result.push prepare_navigation(values, current_path, indent + 2, level + 1)
        result.push "end".indent(indent)
      end
    end
  end.flatten
end

def generate_navigation
  debug "Generating navigation"

  barclamps = Pathname.new(
    File.join(CROWBAR_PATH, "barclamps")
  )

  current = {}

  barclamps.children.each do |barclamp|
    next unless barclamp.extname == ".yml"

    config = YAML.load_file(
      barclamp.to_s
    )

    next if config["nav"].nil?

    current.deep_merge! config["nav"]
  end

  File.open(
    File.join(CROWBAR_PATH, 'config', 'navigation.rb'),
    'w'
  ) do |out|
    out.puts '#'
    out.puts '# Copyright 2011-2013, Dell'
    out.puts '# Copyright 2013-2014, SUSE LINUX Products GmbH'
    out.puts '#'
    out.puts '# Licensed under the Apache License, Version 2.0 (the "License");'
    out.puts '# you may not use this file except in compliance with the License.'
    out.puts '# You may obtain a copy of the License at'
    out.puts '#'
    out.puts '#   http://www.apache.org/licenses/LICENSE-2.0'
    out.puts '#'
    out.puts '# Unless required by applicable law or agreed to in writing, software'
    out.puts '# distributed under the License is distributed on an "AS IS" BASIS,'
    out.puts '# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.'
    out.puts '# See the License for the specific language governing permissions and'
    out.puts '# limitations under the License.'
    out.puts '#'
    out.puts ''
    out.puts 'SimpleNavigation::Configuration.run do |navigation|'
    out.puts '  navigation.renderer = SimpleNavigationRenderers::Bootstrap3'
    out.puts '  navigation.consider_item_names_as_safe = true'
    out.puts ''
    out.puts '  navigation.selected_class = "active"'
    out.puts '  navigation.active_leaf_class = "leaf"'
    out.puts ''
    out.puts '  navigation.items do |level1|'
    out.puts '    level1.dom_class = "nav navbar-nav"'

    out.puts prepare_navigation(current, [], 4, 1).join("\n")

    out.puts '  end'
    out.puts 'end'
  end
end

def generate_assets_manifest
  debug "Generating assets manifest"

  manifests = Pathname.new(
    File.join(CROWBAR_PATH, "barclamps", "manifests")
  )

  merged_json = {}

  manifests.children.each do |manifest|
    next unless manifest.extname == ".json"
    json = JSON.parse (File.open(manifest.to_s, 'r').read())
    merged_json.deep_merge!(json) unless json.nil?
  end

  assets_folder = File.join(CROWBAR_PATH, 'public', 'assets')
  unless File.directory? assets_folder
    FileUtils.mkdir_p assets_folder
  end

  File.open(
    File.join(CROWBAR_PATH, 'public', 'assets', 'manifest.json'),
    'w'
  ) do |out|
    JSON.dump(merged_json, out)
  end
end

# copies paths from one place to another (recursive)
def bc_cloner(item, bc, entity, source, target, replace)
  debug "bc_cloner method called with debug option enabled"
  debug "bc_cloner args: item=#{item}, bc=#{bc}, entity=#{entity}, source=#{source}, target=#{target}, replace=#{replace}"

  files = []
  new_item = (replace ? bc_replacer(item.dup, bc, entity) : item)
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

# Fix file permissions. Note: This doesn't change directory permissions
def chmod_dir(value, path)
  f = Dir.entries(path).find_all { |e| !e.start_with? '.'}
  f.each do |i|
    file = File.join(path,i)
    if File.directory? file
      debug "\tchmod_dir: #{file} is a directory. Skipping it."
    elsif File.exists? file
      FileUtils.chmod value, file
      debug "\tchmod 0#{value.to_s(8)} for #{file}"
    else
      puts "chmod_dir: WARN: missing file #{file} for chmod #{value} operation."
    end
  end
end

# remove model placeholders
def bc_replacer(item, bc, entity)
  debug "bc_replacer method called with debug option enabled"
  debug "bc_replacer args: item=#{item}, bc=#{bc}, entity=#{entity}"

  new_item = item.clone
  new_item.gsub!(MODEL_SUBSTRING_BASE, bc)
  new_item.gsub!(MODEL_SUBSTRING_CAMEL, bc.camelize)
  new_item.gsub!(MODEL_SUBSTRING_HUMAN, bc.humanize)
  new_item.gsub!(MODEL_SUBSTRING_CAPSS, bc.capitalize)
  new_item.gsub!('Copyright 2011, Dell', "Copyright #{Time.now.year}, #{entity}")
  debug "bc_replacer returns new_item=#{new_item}"
  return new_item
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
def bc_remove_layout_1(from_rpm, bc, bc_path, yaml)
  filelist = File.join BARCLAMP_PATH, "#{bc}-filelist.txt"
  if File.exist? filelist
    files = [ filelist ]
    File.open(filelist, 'r') do |f|
      f.each_line { |line| FileUtils.rm line.chomp rescue nil }
    end
    FileUtils.rm filelist rescue nil

    generate_navigation
    generate_assets_manifest
    catalog bc_path

    debug "Barclamp #{bc} UNinstalled"
  end
end

def framework_permissions(bc, bc_path)
  FileUtils.chmod 0755, File.join(CROWBAR_PATH, 'db')
  chmod_dir 0644, File.join(CROWBAR_PATH, 'db')
  FileUtils.chmod 0755, File.join(CROWBAR_PATH, 'tmp')
  chmod_dir 0644, File.join(CROWBAR_PATH, 'tmp')
  debug "\tcopied crowbar_framework files"
end

# install the framework files for a barclamp
# N.B. if you update this, you must also update Guardfile.tree-merge !!
def bc_install_layout_1_app(from_rpm, bc, bc_path, yaml)

  #TODO - add a roll back so there are NOT partial results if a step fails
  files = []

  puts "Installing barclamp #{bc} from #{bc_path}"

  #copy the rails parts (required for render BEFORE import into chef)
  dirs = Dir.entries(bc_path)
  debug "path entries #{dirs.pretty_inspect}"

  unless from_rpm
    # copy all the files to the target

    if dirs.include? 'crowbar_framework'
      debug "path entries include \"crowbar_framework\""
      files += bc_cloner('crowbar_framework', bc, nil, bc_path, BASE_PATH, false)
      framework_permissions bc, bc_path
    end

    if dirs.include? 'bin'
      debug "path entries include \"bin\""
      files += bc_cloner('bin', bc, nil, bc_path, BASE_PATH, false)
      FileUtils.chmod_R 0755, BIN_PATH
      debug "\tcopied command line files"
    end

    if dirs.include? 'chef'
      debug "path entries include \"chef\""
      files += bc_cloner('chef', bc, nil, bc_path, BASE_PATH, false)
      debug "\tcopied over chef parts from #{bc_path} to #{BASE_PATH}"
    end
  end

  # we don't install these files in the right place from rpm
  if dirs.include? 'updates'
    debug "path entries include \"updates\""
    files += bc_cloner('updates', bc, nil, bc_path, ROOT_PATH, false)
    FileUtils.chmod_R 0755, UPDATE_PATH
    debug "\tcopied updates files"
  end

  # copy over the crowbar YAML file, needed to update catalog
  yml_barclamp = get_crowbar_yml_path(bc_path)
  yml_path = File.join CROWBAR_PATH, 'barclamps'
  yml_created = File.join(yml_path, "#{bc}.yml")
  FileUtils.mkdir yml_path unless File.directory? yml_path
  FileUtils.cp yml_barclamp, yml_created
  files << yml_created

  # copy over the manifest.json file, needed to update assets manifest
  manifest_path = File.join CROWBAR_PATH, 'barclamps', 'manifests'
  manifest_barclamp = File.join bc_path, "manifest.json"
  manifest_created = File.join(manifest_path, "#{bc}.json")
  FileUtils.mkdir manifest_path unless File.directory? manifest_path
  if File.exists? manifest_barclamp
    FileUtils.cp manifest_barclamp, manifest_created
    files << manifest_created
  else
    # make sure there's no old manifest
    FileUtils.rm manifest_created rescue nil
  end

  filelist = File.join BARCLAMP_PATH, "#{bc}-filelist.txt"
  File.open( filelist, 'w' ) do |out|
    files.each { |line| out.puts line }
  end

  # Migrate base crowbar schema if needed
  bc_schema_version = yaml["crowbar"]["proposal_schema_version"].to_i rescue 1
  latest_version    = 3

  if bc_schema_version < latest_version
    name = yaml['barclamp']['name']
    schema_file = File.join BASE_PATH, 'chef','data_bags','crowbar', "bc-template-#{name}.schema"
    if File.exists? schema_file
      a = []
      File.open(schema_file, 'r') { |f|
        a = f.readlines
      }
      need_status = bc_schema_version < 2 && a.grep(/^\s+\"crowbar-status\"/).empty?
      need_failed = bc_schema_version < 2 && a.grep(/^\s+\"crowbar-failed\"/).empty?
      need_applied = bc_schema_version < 3 && a.grep(/^\s+\"crowbar-applied\"/).empty?
      if need_status or need_failed or need_applied
        File.open(schema_file, 'w') { |f|
          a.each do |line|
            f.write(line)
            if line =~ /crowbar-queued/
              f.write("            \"crowbar-status\": { \"type\": \"str\" },\n") if need_status
              f.write("            \"crowbar-failed\": { \"type\": \"str\" },\n") if need_failed
              f.write("            \"crowbar-applied\": { \"type\": \"bool\" },\n") if need_applied
            end
          end
        }
      end
    end
  end

  debug "Barclamp #{bc} (format v1) added to Crowbar Framework.  Review #{filelist} for files created."
end

# upload the chef parts for a barclamp
def bc_install_layout_1_chef(from_rpm, bc, bc_path, yaml)
  log_path = File.join '/var', 'log', 'crowbar', 'barclamp_install'
  FileUtils.mkdir log_path unless File.directory? log_path
  log = File.join log_path, "#{bc}.log"
  File.open(log, "a") { |f| f.puts("======== Installing #{bc} barclamp -- #{Time.now.strftime('%c')} ========") }
  debug "Capturing chef install logs to #{log}"
  chef = File.join bc_path, 'chef'
  cookbooks = File.join chef, 'cookbooks'
  databags = File.join chef, 'data_bags'
  roles = File.join chef, 'roles'

  yaml_with_id = yaml.clone
  yaml_with_id["id"] = bc
  begin
    temp = Tempfile.new(["#{bc}-", '.json'])
    temp.write(JSON.pretty_generate(yaml_with_id))
    temp.flush
    upload_data_bag_from_file 'barclamps', temp.path, bc_path, log
  ensure
    temp.close!
  end

  do_migrate = check_schema_migration(bc)

  if from_rpm
    rpm = 'crowbar-' + File.basename(bc_path)
    debug "obtaining chef components from #{rpm} rpm"
    rpm_files = get_rpm_file_list(rpm)
    upload_cookbooks_from_rpm rpm, rpm_files, bc_path, log
    upload_data_bags_from_rpm rpm, rpm_files, bc_path, log
    upload_roles_from_rpm     rpm, rpm_files, bc_path, log
  else
    debug "obtaining chef components from #{bc_path} directory"
    upload_cookbooks_from_dir cookbooks, ['ALL'], bc_path, log
    upload_data_bags_from_dir databags, bc_path, log
    upload_roles_from_dir     roles,    bc_path, log
  end

  puts "Barclamp #{bc} (format v1) Chef Components Uploaded."

  if do_migrate
    debug "Migrating schema to new revision..."
    File.open(log, "a") { |f| f.puts("======== Migrating #{bc} barclamp -- #{Time.now.strftime('%c')} ========") }
    migrate_cmd = "cd #{CROWBAR_PATH} && ./bin/rake --silent crowbar:schema_migrate_prod[#{bc}] 2>&1"
    migrate_cmd_su = "su -s /bin/sh - crowbar sh -c \"#{migrate_cmd}\" >> #{log}"
    debug "running #{migrate_cmd_su}"
    unless system migrate_cmd_su
      fatal "Failed to migrate barclamp #{bc} to new schema revision.", log
    end
    debug "\t executed: #{migrate_cmd_su}"
    puts "Barclamp #{bc} (format v1) Chef Components Migrated."
  else
    debug "No need to migrate schema to new revision"
  end
end

def check_schema_migration(bc)
  template_file = File.join BASE_PATH, 'chef', 'data_bags', 'crowbar', "bc-template-#{bc}.json"
  debug "Looking for new schema-revision in #{template_file}..."
  new_schema_revision = nil
  begin
    if File.exists? template_file
      template = JSON::load File.open(template_file, 'r')
      new_schema_revision = template["deployment"][bc]["schema-revision"]
      debug "New schema-revision for #{bc} is #{new_schema_revision}"
    end
  rescue StandardError
    # pass
  end
  debug "No new schema-revision found for #{bc}" if new_schema_revision.nil?

  debug "Looking for previous schema-revision..."
  old_schema_revision = nil
  begin
    old_json = `knife data bag show -F json crowbar bc-template-#{bc} -k /etc/chef/webui.pem -u chef-webui 2> /dev/null`
    if $?.success?
      template = JSON::load old_json
      old_schema_revision = template["deployment"][bc]["schema-revision"]
      debug "Previous schema-revision for #{bc} is #{old_schema_revision}"
    else
      debug "Failed to retrieve bc-template-#{bc}, no migration necessary"
      return false
    end
  rescue StandardError
    # pass
  end
  debug "No previous schema-revision found for #{bc}" if old_schema_revision.nil?

  return old_schema_revision != new_schema_revision
end

def get_rpm_file_list(rpm)
  cmd = "rpm -ql #{rpm}"
  file_list = `#{cmd}`.lines.map { |line| line.rstrip }
  raise cmd + " failed" unless $? == 0
  debug "obtained file list from #{rpm} rpm"
  return file_list
end

def upload_cookbooks_from_rpm(rpm, rpm_files, bc_path, log)
  cookbooks_dir = "#{BASE_PATH}/chef/cookbooks"
  cookbooks = rpm_files.inject([]) do |acc, file|
    if File.directory?(file) and file =~ %r!^#{cookbooks_dir}/([^/]+)$!
      cookbook = File.basename(file)
      debug "will upload #{cookbook} from #{file} from #{rpm} rpm"
      acc.push cookbook
    end
    acc
  end
  if cookbooks.empty?
    puts "WARNING: didn't find any cookbooks from #{rpm} rpm in #{cookbooks_dir}"
  else
    upload_cookbooks_from_dir(cookbooks_dir, cookbooks, bc_path, log)
  end
end

def upload_data_bags_from_rpm(rpm, rpm_files, bc_path, log)
  data_bags_dir = "#{BASE_PATH}/chef/data_bags"
  data_bag_files = rpm_files.grep(%r!^#{data_bags_dir}/([^/]+)/[^/]+\.json$!) do |path|
    [ $1, path ]
  end
  if data_bag_files.empty?
    puts "WARNING: didn't find any data bags from #{rpm} rpm in #{data_bags_dir}"
  else
    data_bag_files.each do |bag, bag_item_path|
      debug "uploading #{bag} from #{rpm} rpm"
      upload_data_bag_from_file(bag, bag_item_path, bc_path, log)
    end
  end
end

def upload_roles_from_rpm(rpm, rpm_files, bc_path, log)
  roles_dir = "#{BASE_PATH}/chef/roles"
  roles = rpm_files.grep(%r!^#{roles_dir}/([^/]+)$!)
  if roles.empty?
    puts "WARNING: didn't find any roles from #{rpm} rpm in #{roles_dir}"
  else
    roles.each do |role|
      upload_role_from_dir(role, bc_path, log)
    end
  end
end

def upload_cookbooks_from_dir(cookbooks_dir, cookbooks, bc_path, log)
  upload_all = cookbooks.length == 1 && cookbooks[0] == 'ALL'
  if File.directory? cookbooks_dir
    FileUtils.cd cookbooks_dir
    opts = upload_all ? '-a' : cookbooks.join(' ')
    knife_cookbook = "knife cookbook upload -o . #{opts} -V -k /etc/chef/webui.pem -u chef-webui"
    debug "running #{knife_cookbook} from #{cookbooks_dir}"
    unless system knife_cookbook + " >> #{log} 2>&1"
      fatal "#{bc_path} #{knife_cookbook} upload failed.", log
    end
    debug "\texecuted: #{bc_path} #{knife_cookbook}"
  else
    debug "\tNOTE: could not find cookbooks dir #{cookbooks_dir}"
  end
end

def upload_data_bags_from_dir(databags_dir, bc_path, log)
  if File.exists? databags_dir
    Dir.entries(databags_dir).each do |bag|
      next if bag == "." or bag == ".."
      bag_path = File.join databags_dir, bag
      FileUtils.chmod 0755, bag_path
      chmod_dir 0644, bag_path
      upload_data_bag_from_dir bag, bag_path, bc_path, log
    end
  else
    debug "\tNOTE: could not find data bags dir #{databags_dir}"
  end
end

# Upload data bag items from any JSON files in the provided directory
def upload_data_bag_from_dir(bag, bag_path, bc_path, log)
  json = Dir.glob(bag_path + '/*.json')
  json.each do |bag_item_path|
    upload_data_bag_from_file(bag, bag_item_path, bc_path, log)
  end
end

def create_data_bag(bag, log, bc_path)
  knife_bag  = "knife data bag create #{bag} -V -k /etc/chef/webui.pem -u chef-webui"
  unless system knife_bag + " >> #{log} 2>&1"
    fatal "#{knife_bag} failed.", log
  end
  debug "\texecuted: #{bc_path} #{knife_bag}"
end

def upload_data_bag_from_file(bag, bag_item_path, bc_path, log)
  create_data_bag(bag, log, bc_path)

  knife_databag  = "knife data bag from file #{bag} #{bag_item_path} -V -k /etc/chef/webui.pem -u chef-webui"
  unless system knife_databag + " >> #{log} 2>&1"
    fatal "#{knife_databag} failed.", log
  end
  debug "\texecuted: #{bc_path} #{knife_databag}"
end

def upload_roles_from_dir(roles, bc_path, log)
  if File.directory? roles
    FileUtils.cd roles
    Dir[roles + "/*.rb"].each do |role_path|
      upload_role_from_dir(role_path, bc_path, log)
    end
  else
    debug "\tNOTE: could not find roles dir #{roles}"
  end
end

def upload_role_from_dir(role_path, bc_path, log)
  debug "will upload #{role_path}"
  knife_role = "knife role from file #{role_path} -V -k /etc/chef/webui.pem -u chef-webui"
  unless system knife_role + " >> #{log} 2>&1"
    fatal "#{knife_role} failed.", log
  end
  debug "\texecuted: #{bc_path} #{knife_role}"
end

def bc_install_layout_1_cache(from_rpm, bc, bc_path, yaml)
  return unless File.directory?(File.join(bc_path,"cache"))
  Dir.entries(File.join(bc_path,"cache")).each do |ent|
    debug ent.inspect
    case
    when ent == "files"
      debug "Copying files"
      system "cp -r \"#{bc_path}/cache/#{ent}\" /tftpboot"
    when ent == "gems"
      # Symlink the gems into One Flat Directory.
      debug "Installing gems"
      Dir.entries("#{bc_path}/cache/gems").each do |gem|
        next unless /\.gem$/ =~ gem
        unless File.directory? "/tftpboot/gemsite/gems"
          system "mkdir -p /tftpboot/gemsite/gems"
        end
        unless File.symlink? "/tftpboot/gemsite/gems/#{gem}"
          debug "Symlinking #{bc_path}/cache/gems/#{gem} into /tftpboot/gemsite/gems"
          File.symlink "#{bc_path}/cache/gems/#{gem}", "/tftpboot/gemsite/gems/#{gem}"
        end
      end
      debug "Done"
    when File.directory?("#{bc_path}/cache/#{ent}/pkgs")
      debug "Installing packages"
      # We have actual packages here.  They map into the target like so:
      # bc_path/ent/pkgs -> /tftboot/ent/crowbar-extras/bc
      unless File.directory? "/tftpboot/#{ent}/crowbar-extra/"
        system "mkdir -p \"/tftpboot/#{ent}/crowbar-extra/\""
      end
      # sigh, ubuntu-install and redhat-install.
      unless File.symlink? "/tftpboot/#{ent}/crowbar-extra/#{bc_path.split('/')[-1]}"
        debug "Symlinking #{bc_path}/cache/#{ent}/pkgs into /tftpboot/#{ent}/crowbar-extra"
        File.symlink "#{bc_path}/cache/#{ent}/pkgs", "/tftpboot/#{ent}/crowbar-extra/#{bc_path.split('/')[-1]}"
      end
    else
      # Symlink the repos into One Flat Directory for serving.
      FileUtils.mkdir_p "/tftpboot/#{ent}"
      unless File.symlink? "/tftpboot/#{ent}/#{bc}"
        File.symlink("#{bc_path}/cache/#{ent}","/tftpboot/#{ent}/#{bc}")
      end
    end
    debug "Done"
    true
  end
end
