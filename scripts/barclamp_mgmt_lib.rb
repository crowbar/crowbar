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

require "rubygems"
require "fileutils"
require "yaml"
require "json"
require "time"
require "tempfile"
require "active_support/all"
require "pp"
require "i18n"
require "pathname"
require "etc"

if I18n.respond_to? :enforce_available_locales
  I18n.enforce_available_locales = false
end

MODEL_SUBSTRING_BASE = '==BC-MODEL=='
MODEL_SUBSTRING_CAMEL = '==^BC-MODEL=='
MODEL_SUBSTRING_HUMAN = '==*BC-MODEL=='
MODEL_SUBSTRING_CAPSS = '==%BC-MODEL=='

if ENV["CROWBAR_DIR"]
  BASE_PATH = ENV["CROWBAR_DIR"]
else
  BASE_PATH = File.join("/opt", "dell").freeze
end
BARCLAMP_PATH = File.join(BASE_PATH, "barclamps").freeze
CROWBAR_PATH = File.join(BASE_PATH, "crowbar_framework").freeze
MODEL_SOURCE = File.join(CROWBAR_PATH, "barclamp_model").freeze
BIN_PATH = File.join(BASE_PATH, "bin").freeze
UPDATE_PATH = "/updates".freeze
ROOT_PATH = "/".freeze

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

def get_yml_paths_from_rpm(component)
  rpm = "crowbar-#{component}"
  get_rpm_file_list(rpm).select do |file|
    file =~ %r!^#{CROWBAR_PATH}/barclamps/([^/]+).yml$!
  end
end

def get_yml_paths(directory, suggested_bc_name = nil)
  yml_files = Array.new
  Dir.entries(directory).each do |file_name|
    path = File.join(directory, file_name)
    if file_name.end_with?("#{suggested_bc_name}.yml") and File.exists?(path)
      yml_files.push path
    end
  end
  yml_files
end

# regenerate the barclamp catalog (does a complete regen each install)
def catalog
  debug "Creating catalog"
  # create the groups for the catalog - for now, just groups.  other catalogs may be added later
  cat = { 'barclamps'=>{} }
  barclamps = File.join CROWBAR_PATH, 'barclamps'
  list = Dir.entries(barclamps).find_all { |e| !e.start_with?(".") && e.end_with?(".yml") }
  # scan the installed barclamps
  list.each do |bc_file|
    debug "Loading #{bc_file}"
    bc = YAML.load_file File.join(barclamps, bc_file)
    name =  bc['barclamp']['name']
    cat['barclamps'][name] = {} if cat['barclamps'][name].nil?
    description = bc['barclamp']['description']
    puts "Warning: Barclamp #{name} has no description!" if description.nil?
    display = bc['barclamp']['display']
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

  config_path = Pathname.new(CROWBAR_PATH).join("config")
  config_path.mkpath unless config_path.directory?

  config_path.join("navigation.rb").open("w") do |out|
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

  manifests = Pathname.new(CROWBAR_PATH).join("barclamps", "manifests")

  merged_json = {}

  manifests.children.each do |manifest|
    next unless manifest.extname == ".json"
    json = JSON.parse (File.open(manifest.to_s, 'r').read())
    merged_json.deep_merge!(json) unless json.nil?
  end

  assets_path = Pathname.new(CROWBAR_PATH).join("public", "assets")
  assets_path.mkpath unless assets_path.directory?

  assets_path.join("manifest.json").open("w") do |out|
    JSON.dump(merged_json, out)
  end
end

# copies paths from one place to another (recursive)
def bc_cloner(item, entity, source, target)
  debug "bc_cloner method called with debug option enabled"
  debug "bc_cloner args: item=#{item}, entity=#{entity}, source=#{source}, target=#{target}"

  files = []
  debug "item=#{item}"
  new_file = File.join target, item
  debug "new_file=#{new_file}"
  new_source = File.join(source, item)
  debug "new_source=#{new_source}"
  if File.directory? new_source
    debug "\tcreating directory #{new_file}."
    FileUtils.mkdir new_file unless File.directory? new_file
    clone = Dir.entries(new_source).find_all { |e| !e.start_with? '.'}
    clone.each do |recurse|
      files += bc_cloner(recurse, entity, new_source, new_file)
    end
  else
    #need to inject into the file
    debug "\t\tcopying file #{new_file}."
    FileUtils.cp new_source, new_file
    files.push(new_file)
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
def bc_remove_layout_1(from_rpm, component)
  filelist = File.join BARCLAMP_PATH, "#{component}-filelist.txt"
  if File.exist? filelist
    File.open(filelist, 'r') do |f|
      f.each_line { |line| FileUtils.rm line.chomp rescue nil }
    end
    FileUtils.rm filelist rescue nil

    debug "Component #{component} Uninstalled"
  end
end

def framework_permissions
  FileUtils.chmod 0755, File.join(CROWBAR_PATH, 'db')
  chmod_dir 0644, File.join(CROWBAR_PATH, 'db')
  FileUtils.chmod 0755, File.join(CROWBAR_PATH, 'tmp')
  chmod_dir 0644, File.join(CROWBAR_PATH, 'tmp')
  debug "\tcopied crowbar_framework files"
end

# install the framework files for a component
# N.B. if you update this, you must also update Guardfile.tree-merge !!
def bc_install_layout_1_app(from_rpm, bc_path)

  #TODO - add a roll back so there are NOT partial results if a step fails
  files = []
  component = File.basename(bc_path)

  puts "Installing component #{component} from #{bc_path}"

  #copy the rails parts (required for render BEFORE import into chef)
  dirs = Dir.entries(bc_path)
  debug "path entries #{dirs.pretty_inspect}"

  unless from_rpm
    # copy all the files to the target

    if dirs.include? "crowbar_framework"
      debug "path entries include \"crowbar_framework\""
      files += bc_cloner("crowbar_framework", nil, bc_path, BASE_PATH)
      framework_permissions
    end

    if dirs.include? "bin"
      debug "path entries include \"bin\""
      files += bc_cloner("bin", nil, bc_path, BASE_PATH)
      FileUtils.chmod_R 0755, BIN_PATH
      debug "\tcopied command line files"
    end

    if dirs.include? "chef"
      debug "path entries include \"chef\""
      files += bc_cloner("chef", nil, bc_path, BASE_PATH)
      debug "\tcopied over chef parts from #{bc_path} to #{BASE_PATH}"
    end

    # copy over the crowbar YAML files, needed to update catalog
    yml_path = File.join CROWBAR_PATH, "barclamps"
    get_yml_paths(bc_path).each do |yml_source|
      yml_created = File.join(yml_path, File.basename(yml_source))
      FileUtils.mkdir yml_path unless File.directory? yml_path
      FileUtils.cp yml_source, yml_created unless yml_source == yml_created
      files.push(yml_created)
    end
  end

  # we don't install these files in the right place from rpm
  if dirs.include? 'updates'
    debug "path entries include \"updates\""
    files += bc_cloner("updates", nil, bc_path, ROOT_PATH)
    FileUtils.chmod_R 0755, UPDATE_PATH
    debug "\tcopied updates files"
  end

  filelist = File.join BARCLAMP_PATH, "#{component}-filelist.txt"
  File.open( filelist, 'w' ) do |out|
    files.each { |line| out.puts line }
  end

  debug "Component #{component} added to Crowbar Framework.  Review #{filelist} for files created."
end

def run_rake_task(task_name, log)
  crowbar_user = Etc.getpwnam("crowbar")
  rails_env = ENV["RAILS_ENV"] || "production"
  debug("spawning bin/rake #{task_name}")
  rake_process = spawn({ "RAILS_ENV" => rails_env },
                       "bin/rake --silent #{task_name}",
                       :uid => crowbar_user.uid,
                       :gid => crowbar_user.gid,
                       :chdir => CROWBAR_PATH,
                       [:out, :err] => [log, "a"])
  Process.wait rake_process
  status = $?
  debug("bin/rake exited with #{status.exitstatus}")
  status.success?
end

# sync the chef parts
def bc_install_layout_1_chef(log)
  unless run_rake_task("chef:upload:all", log)
    fatal "Failed to upload cookbooks to chef.", log
  end
end

def bc_install_schema_migrate(barclamps, log)
  debug "Migrating barclamps #{barclamps.join(", ")} to new schema revision..."
  File.open(log, "a") { |f| f.puts("======== Migrating #{barclamps.join(", ")} barclamps -- #{Time.now.strftime('%c')} ========") }
  unless run_rake_task("crowbar:schema_migrate[#{barclamps.join(",")}]", log)
    fatal "Failed to migrate barclamps #{barclamps.join(", ")} to new schema revision.", log
  end
  puts "Barclamps #{barclamps.join(", ")} migrated to New Schema Revision."
end

def get_rpm_file_list(rpm)
  cmd = "rpm -ql #{rpm}"
  file_list = `#{cmd}`.lines.map { |line| line.rstrip }
  raise cmd + " failed" unless $? == 0
  debug "obtained file list from #{rpm} rpm"
  return file_list
end
