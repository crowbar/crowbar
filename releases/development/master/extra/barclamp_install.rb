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
require 'getoptlong'
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

  def skip_engines=(t)
    @skip_engines = !!t
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
      next if /^\./ =~ ent or ent == "debian"
      case ent
      when "crowbar_framework"
        next if use_engine || @skip_files
        debug("Copying crowbar_framework files over for #{bc}")
        FileUtils.cp_r(File.join(@source,'crowbar_framework'),@target)
      when "crowbar_engine"
        FileUtils.cp_r(File.join(@source,ent),@barclamp_dir) unless @source == @barclamp_dir
        if FileTest.exists?(File.join(@barclamp_dir,ent,"barclamp_"+bc+"/test/unit"))
          FileUtils.mkdir_p(File.join(@target,"crowbar_framework/test/unit"))
          FileUtils.cp_r(File.join(@barclamp_dir,ent,"barclamp_"+bc+"/test/unit"),File.join(@target,"crowbar_framework/test/"))
        end
        next if @skip_engines 
        debug("#{bc} is implemented using a Rails Engine.")
        debug("Linking in routes and Gemfile entries.")
        system("perl -pi -e 's|engine mounts|engine mounts\n  mount Barclamp" + camelize(bc) + "::Engine, :at => \"" + bc + "\"|' " + File.join(@target,'crowbar_framework','config','routes.rb'))
        system("perl -pi -e 's|engine mounts|engine mounts\ngem \"barclamp_" + bc + "\", :path => \"" + File.join(@barclamp_dir,'crowbar_engine',"barclamp_#{bc}") + "\"|' " + File.join(@target,'crowbar_framework','Gemfile'))
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
      debug "#{Dir.pwd}: Installing migrations for #{bc}"
      system "su -s /bin/bash -c 'RAILS_ENV=production bundle exec rake railties:install:migrations' crowbar"
      debug "Running migrations for #{bc}"
      db = system "su -s /bin/bash -c 'RAILS_ENV=production bundle exec rake db:migrate' crowbar"
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
    FileUtils.cd(cookbooks) do
      debug("Uploading chef cookbooks for #{bc}")
      unless knife("cookbook upload -o . --all")
        fatal("Could not upload cookbooks from #{cookbooks}")
      end
    end if File.directory?(cookbooks)
    data_bags = File.join(chef,'data_bags')
    FileUtils.cd(data_bags) do
      debug("Uploading chef data bags for #{bc}")
      Dir.entries(".").each do |bag|
        next if bag == '.' or bag == '..'
        knife("data bag create \"#{bag}\"")
        unless knife("data bag from file \"#{bag}\" \"#{bag}\"")
          fatal("Could not upload data bag #{bag}")
        end
      end
    end if File.directory?(data_bags)
    roles = File.join(chef,'roles')
    FileUtils.cd(roles) do
      debug("Uploading chef roles for #{bc}")
      unless knife("role from file *.rb")
        fatal("Could not upload role #{role}")
      end
    end if File.directory?(roles)
  end

  def run_actions(stage)
    return if @skip_install_actions
    setup_dir = File.join(@barclamp_dir,'setup')
    return unless File.directory?(setup_dir)
    FileUtils.cd(setup_dir) do
      Dir.glob("*.#{stage}").sort.each do |action|
        next unless File.executable?(action)
        next if system("CROWBAR_DIR=\"#{@target}\" BC_PATH=\"#{@barclamp_dir}\" ./#{action}")
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
    install_cache
    run_actions("install")
    install_chef
  end
end


@@no_rsync = false

opts = GetoptLong.new(
  [ '--help', '-h', GetoptLong::NO_ARGUMENT ],
  [ '--debug', '-d', GetoptLong::NO_ARGUMENT ],
  [ '--build', GetoptLong::NO_ARGUMENT ],
  [ '--deploy', '-i', GetoptLong::NO_ARGUMENT ],
  [ '--no-files', '-x', GetoptLong::NO_ARGUMENT ],
  [ '--no-engines', '-e', GetoptLong::NO_ARGUMENT ],
  [ '--no-install-actions', '-a', GetoptLong::NO_ARGUMENT ],
  [ '--no-chef', '-c', GetoptLong::NO_ARGUMENT ],
  [ '--base-dir', '-b', GetoptLong::REQUIRED_ARGUMENT ],
  [ '--root', '-r', GetoptLong::REQUIRED_ARGUMENT ],
  [ '--test', '-t', GetoptLong::REQUIRED_ARGUMENT ],
  [ '--force', '-f', GetoptLong::NO_ARGUMENT ]
)

def usage()
  puts "Usage:"
  puts "#{__FILE__} [--help] [--debug] [--no-files] [--no-chef] [--no-install-actions] [--deploy] [--build] [--base-dir <dir>] [--root <dir>] /path/to/new/barclamp"
  exit
end

force_install = false

@@base_dir = "/opt/dell"
@@root = nil
@@no_install_actions = false
@@no_chef = false
@@no_migrations = false
@@no_rsync = false
@@no_files = false
@@no_engines = false

opts.each do |opt, arg|
  case opt
  when "--help" then usage
  when "--debug" then ENV['DEBUG']="true"; debug "debug mode is enabled"
  when "--build"
    @@no_install_actions = true
    @@no_chef = true
    @@no_migrations = true
    @@no_rsync = true
    @@no_engines = true
  when "--no-framework-install" then @@no_framework = true
  when "--no-install-actions" then @@no_install_actions = true
  when "--deploy" then @@deploy = true
  when "--no-files" then @@no_files = true; debug "no-files is enabled"
  when "--no-engines" then @@no_engines = true; debug "no-engine is enabled"
  when "--no-chef" then @@no_chef = true; debug "no-chef is enabled"
  when "--base-dir" then @@base_dir = arg; debug "base-dir is #{@@base_dir}"
  when "--force" then force_install = true
  when "--root" then @@root = arg
  end
end

usage if ARGV.length < 1

tmpdir = "/tmp/bc_install-#{Process.pid}-#{Kernel.rand(65535)}"
debug "tarball tmpdir: #{tmpdir}"
Dir.mkdir(tmpdir)
candidates = Array.new

def rm_tmpdir(tmpdir)
  if File.directory?(tmpdir)
    debug "temporary directory #{tmpdir} will be removed"
    system "rm -rf #{tmpdir}"
  end
end

ARGV.each do |src|
  debug "src: #{src}"
  bc=nil
  case
  when /tar\.gz|tgz$/ =~ src
    # This might be a barclamp tarball.  Expand it into a temporary location.
    src=File.expand_path(src)
    system "tar xzf \"#{src}\" -C \"#{tmpdir}\""
    target="#{tmpdir}/#{src.split("/")[-1].split(".")[0]}"
    if File.exists?(File.join(target,"crowbar.yml"))
      bc = BarclampFS.new(target,@@base_dir,@@root)
    else
      puts "#{src} is not a barclamp tarball, ignoring."
      next
    end
  when File.exists?(File.join(src,"crowbar.yml"))
    # We were handed something that looks like a path to a barclamp
    bc = BarclampFS.new(File.expand_path(src),@@base_dir,@@root)
  when File.exists?(File.join(@@base_dir,"barclamps",src,"crowbar.yml"))
    bc = BarclampFS.new(File.join(@@base_dir,"barclamps",src),@@base_dir,@@root)
  else
    debug "base directory is #{@@base_dir}"
    puts "#{src} is not a barclamp, ignoring."
    next
  end
  unless bc && bc.bc
    puts "Barclamp at #{src} has no name, skipping"
    next
  end
  bc.skip_chef = @@no_chef
  bc.skip_files = @@no_files
  bc.skip_install_actions = @@no_install_actions
  bc.skip_migrations = @@no_migrations
  bc.skip_engines = @@no_engines
  candidates << bc if bc
end

debug "installing barclamps:"
candidates.sort_by{|c| c["crowbar"]["order"].to_i rescue 9999}.each do |bc|
  debug bc.bc
  begin
    unless bc.source == bc.dir
      if File.directory?(bc.dir)
        debug("target directory #{bc.dir} exists")
        if File.exists?("#{bc.dir}/crowbar.yml")
          debug("#{bc.dir}/crowbar.yml file exists")
          if File.exists?("#{bc.dir}/sha1sums")
            debug("#{bc.dir}/sha1sums file exists")
            unless force_install or system "cd \"#{bc.dir}\"; sha1sum --status -c sha1sums"
              debug "force_install mode is disabled and not all file checksums do match"
              puts "Refusing to install over non-pristine target #{bc.dir}"
              puts "Please back up the following files:"
              system "cd \"#{bc.dir}\"; sha1sum -c sha1sums |grep -v OK"
              puts "and rerun the install after recreating the checksum file with:"
              puts "  cd \"#{bc.dir}\"; find -type f -not -name sha1sums -print0 | \\"
              puts "       xargs -0 sha1sum -b >sha1sums"
              puts "(or use the --force switch)"
              debug "temporary directory #{tmpdir} will be removed if it exists"
              system "rm -rf #{tmpdir}" if File.directory?(tmpdir)
              exit 1
            end
          elsif not force_install
            debug "force_install mode is disabled and #{bc.dir}/sha1sums file does not exist"
            puts "#{bc.dir} already exists, but it does not have checksums."
            puts "Please back up any local changes you may have made, and then"
            puts "create a checksums file with:"
            puts "  cd \"#{bc.dir}\"; find -type f -not -name sha1sums -print0 | \\"
            puts "       xargs -0 sha1sum -b >sha1sums"
            puts "(or use the --force switch)"
            debug "temporary directory #{tmpdir} will be removed if it exists"
            system "rm -rf #{tmpdir}" if File.directory?(tmpdir)
            exit 1
          end
        else
          debug "#{bc.dir}/crowbar.yml does not exist"
          puts "#{bc.dir} exists, but it is not a barclamp."
          puts "Cowardly refusing to overwrite it."
          debug "temporary directory #{tmpdir} will be removed if it exists"
          system "rm -rf #{tmpdir}" if File.directory?(tmpdir)
          exit 1
        end
      end
    end
    debug "installing barclamp"
    begin
      bc.install
    rescue StandardError => e
      debug "exception occurred while installing barclamp"
      raise e
    end
  rescue StandardError => e
    if ENV['DEBUG'] == "true"
      debug "temporary directory #{tmpdir} will be left for debugging if it exists"
    else
      rm_tmpdir(tmpdir)
    end
    puts e
    puts e.backtrace
    puts "Install of #{bc[:name]} failed."
    exit -3
  end
end

rm_tmpdir(tmpdir)

exit 0
