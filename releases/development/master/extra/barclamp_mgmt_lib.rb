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
#

require 'rubygems'
require 'yaml'
require 'json'
require 'fileutils'
require 'pp'

def debug(msg)
  STDERR.puts "DEBUG: " + msg if ENV['DEBUG'] == "true"
end

def fatal(msg, log=nil)
  m = "ERROR: #{msg}  Aborting"
  m ="#{m}; examine #{log} for more info."  if log
  STDERR.puts m
  exit 1
end

def camelize(str)
  str.split('_').map {|w| w.capitalize}.join
end

def knife(args)
  system("knife #{args} -k /etc/chef/webui.pem -u chef-webui")
end

class BarclampFS
  attr_reader :source, :target, :chroot, :root
  attr_reader :skip_chef, :skip_files, :skip_migrations, :skip_install_actions
  attr_reader :metadata
  def initialize(src, tgt, root)
    if root
      @chroot=true
      @root=root
    else
      @root='/'
      @target=tgt
    end
    @skip_chef=false | @chroot
    @skip_migrations=false | @chroot
    @skip_install_actions=false | @chroot
    @skip_files=false
    @source = src
    @target=File.join(@root,tgt)
    begin
      @metadata=YAML.load_file(File.join(@source,"crowbar.yml"))
    rescue
      fatal("#{@source} is not an unpacked barclamp!")
    end
    @barclamp_dir=File.join(@target,'barclamps',bc)
  end

  def skip_chef=(t)
    @skip_chef = !!t | @chroot
  end

  def skip_files=(t)
    @skip_files = !!t
  end

  def skip_migrations=(t)
    @skip_migrations = !!t | @chroot
  end

  def skip_install_actions=(t)
    @skip_install_actions = !!t | @chroot
  end

  def bc
    @metadata['barclamp']['name'] rescue nil
  end

  def dir
    @barclamp_dir
  end

  def [](k)
    @metadata[k]
  end

  def install_app
    STDERR.puts "Installing #{bc} from #{@source} into #{@target}"
    dirs = Dir.entries(@source)
    use_engine = dirs.include?("crowbar_engine")
    dirs.sort.each do |ent|
      next if /^\./ =~ ent
      case ent
      when "crowbar_framework"
        next if use_engine || @skip_files
        debug("Copying crowbar_framework files over for #{bc}")
        FileUtils.cp_r(File.join(@source,'crowbar_framework'),@target)
      when "crowbar_engine"
        next if @chroot
        debug("#{name} is implemented using a Rails Engine.")
        debug("Linking in routes and Gemfile entries.")
        system("perl -pi -e 's|engine mounts|engine mounts\n  mount Barclamp" + camelize(bc) + "::Engine, :at => \"" + name + "\"|' " + File.join(@target,'crowbar_framework','config','routes.rb'))
        system("perl -pi -e 's|engine mounts|engine mounts\ngem \"barclamp_" + bc + "\", :path => \"" + File.join(@source,'crowbar_engine',"barclamp_#{bc}") + "\"|' " + File.join(@target,'crowbar_framework','Gemfile'))
      when 'bin'
        debug("Installing commands for #{bc}")
        FileUtils.mkdir_p(File.join(@target,'bin'))
        FileUtils.cp_r(File.join(@source,'bin'),@target)
        FileUtils.chmod_R(0755,File.join(@target,'bin'))
      when 'updates'
        debug("Installing Sledgehammer updates for #{bc}")
        FileUtils.mkdir_p(File.join(@root,'updates'))
        FileUtils.cp_r(File.join(@source,'updates'),@root)
      when 'BDD'
        debug("Installing BDD framework components for #{bc}")
        FileUtils.mkdir_p(File.join(@target,'BDD'))
        FileUtils.cp_r(File.join(@source,'BDD'),@target)
      when 'doc'
        debug("Installing documentation for #{bc}")
        FileUtils.mkdir_p(File.join(@target,'doc',bc))
        FileUtils.cp_r(File.join(@source,'doc','.'),File.join(@target,'doc',bc))
      else
        next if @source == @barclamp_dir
        debug("Copying #{ent} into #{@barclamp_dir}")
        FileUtils.mkdir_p(@barclamp_dir)
        FileUtils.cp_r(File.join(@source,ent),@barclamp_dir)
      end
    end
  end

  def install_migrations
    return if @skip_migrations
    FileUtils.cd(File.join(@target,"crowbar_framework")) do
      debug "Installing migrations for #{bc}"
      system "RAILS_ENV=production bundle exec rake railties:install:migrations"
      debug "Running migrations for #{bc}"
      db = system "RAILS_ENV=production bundle exec rake db:migrate"
    end
  end

  def install_chef
    return if @skip_chef
    unless File.exists?('/root/.chef/knife.rb')
      STDERR.puts("Knife not configured. Not installing #{bc} Chef Components.")
      return
    end
    chef=File.join(@barclamp_dir,'chef')
    unless File.directory?(chef)
      STDERR.puts("No chef components to upload")
      return
    end
    cookbooks = File.join(chef,'cookbooks')
    if File.directory?(cookbooks)
      debug("Uploading chef cookbooks for #{bc}")
      unless knife("cookbook upload -o \"#{cookbooks}\"")
        fatal("Could not upload cookbooks from #{cookbooks}")
      end
    end
    data_bags = File.join(chef,'data_bags')
    if File.directory?(data_bags)
      debug("Uploading chef data bags for #{bc}")
      Dir.entries(data_bags).each do |bag|
        next if bag == '.' or bag == '..'
        knife("data bag create \"#{bag}\"")
        Dir.glob(File.join(data_bags,bag,'*.json')).each do |ent|
          unless knife("data bag from file \"#{bag}\" \"#{ent}\"")
            fatal("Could not upload data bag #{ent}")
          end
        end
      end
    end
    roles = File.join(chef,'roles')
    if File.directory?(roles)
      debug("Uploading chef roles for #{bc}")
      Dir.glob(File.join(roles,'*.rb')).each do |role|
        unless knife("role from file \"#{role}\"")
          fatal("Could not upload role #{role}")
        end
      end
    end
  end

  def run_actions(stage)
    return if @skip_install_actions
    setup_dir = File.join(@barclamp_dir,'setup')
    return unless File.directory?(setup_dir)
    FileUtils.cd(setup_dir) do
      Dir.glob("*.#{stage}").sort.each do |action|
        next unless File.executable?(action)
        next if system("CROWBAR_DIR=\"#{@target}\" BC_PATH=\"#{@barclamp_dir}\" #{action}")
        fatal("#{action} exited with non-zero status.")
      end
    end
  end

  def install_cache
    cache = File.join(@source,'cache')
    return unless File.directory?(cache)
    Dir.entries(cache).each do |ent|
      case
      when ent == 'files'
        debug("Copying cache files from #{cache} into /tftpboot/files")
        FileUtils.cp_r(File.join(cache,'files'),'/tftpboot')
      when ent == 'gems'
        debug("Installing gems from #{cache} into /tftpboot/gemsite")
        FileUtils.mkdir_p('/tftpboot/gemsite/gems')
        Dir.entries(File.join(cache,'gems')).each do |gem|
          next unless /\.gem$/ =~ gem
          next if File.symlink?(File.join('/tftpboot/gemsite/gems',gem))
          FileUtils.ln_s(File.join(cache,'gems',gem),File.join('/tftpboot/gemsite/gems',gem))
        end
      when File.directory?(File.join(cache,ent,'pkgs'))
        debug "Installing packages from #{cache} into /tftpboot/#{ent}/crowbar-extra/"
        src=File.join(cache,ent,'pkgs')
        tgt=File.join('/tftpboot',ent,'crowbar-extra')
        FileUtils.mkdir_p(tgt)
        tgt=File.join(tgt,bc)
        next if File.symlink?(tgt)
        FileUtils.ln_s(src,tgt)
      end
    end
  end

  def install
    install_app
    install_migrations
    install_chef
    install_cache
    run_actions("install")
  end
end
