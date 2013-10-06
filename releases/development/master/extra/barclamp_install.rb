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

class BarclampFS
  attr_reader :source, :metadata, :name
  attr_accessor :installed
  @@groups = Hash.new
  def initialize(src)
    @installed = false
    @chroot = false
    @skip_files=false
    @root = '/'
    @source = src
    begin
      @metadata=YAML.load_file(File.join(@source,"crowbar.yml"))
    rescue
      fatal("#{@source} is not an unpacked barclamp!")
    end
    @name = @metadata['barclamp']['name']
    if (@metadata['barclamp']['member'] rescue nil)
      @metadata['barclamp']['member'].each do |grp|
        @@groups[grp] ||= Array.new
        @@groups[grp] << @name
      end
    end
  end

  def root=(rootdir)
    @chroot=true
    FileUtils.mkdir_p(rootdir)
    @root=rootdir
  end

  def target=(tgt)
    @target=tgt
  end

  def target
    File.expand_path(File.join(@root,@target))
  end

  def dir
    File.join(target,"barclamps",@name)
  end

  def skip_files=(t)
    @skip_files = !!t
  end

  def skip_files
    @skip_files
  end

  def skip_engines=(t)
    @skip_engines = !!t
  end

  def skip_engines
    @skip_engines
  end

  def skip_migrations=(t)
    @skip_migrations = !!t | @chroot
  end

  def skip_migrations
    @skip_migrations | @chroot
  end

  def skip_install_actions=(t)
    @skip_install_actions = !!t | @chroot
  end

  def skip_install_actions
    @skip_install_actions | @chroot
  end

  def requires
    (@metadata['barclamp']['requires'] rescue []).map do |d|
      if /^@/ =~ d then @@groups[d[1..-1]] else d end
    end.flatten.uniq
  end

  def [](k)
    @metadata[k]
  end

  # N.B. if you update this, you must also update Guardfile.tree-merge !!
  def install_app
    STDERR.puts "Installing #{@name} from #{@source} into #{target}"
    dirs = Dir.entries(@source)
    use_engine = dirs.include?("crowbar_engine")
    dirs.sort.each do |ent|
      next if /^\./ =~ ent or ent == "debian"
      case ent
      when "crowbar_framework"
        next if use_engine || skip_files
        debug("Copying crowbar_framework files over for #{@name}")
        FileUtils.cp_r(File.join(@source,'crowbar_framework'),target)
      when "crowbar_engine"
        FileUtils.cp_r(File.join(@source,ent),dir) unless @source == dir
        if FileTest.exists?(File.join(dir,ent,"barclamp_"+@name+"/test/unit"))
          FileUtils.mkdir_p(File.join(target,"crowbar_framework/test/unit"))
          FileUtils.cp_r(File.join(dir,ent,"barclamp_"+@name+"/test/unit"),File.join(target,"crowbar_framework/test/"))
        end
        if FileTest.exists?(File.join(dir,ent,"barclamp_"+@name+"/test/"+@name+"_test_helper.rb"))
          FileUtils.cp(File.join(dir,ent,"barclamp_"+@name+"/test/"+@name+"_test_helper.rb"),File.join(target,"crowbar_framework/test/"))
        end
        debug("#{@name} is implemented using a Rails Engine.")
        debug("Linking in routes and Gemfile entries.")
        gem_name       = "barclamp_#{@name}"
        engine_class   = "Barclamp%s::Engine" % camelize(@name)
        # N.B. the run-time path to the engine could be different from
        # the install-time path referenced by our 'dir' variable, so
        # instead we use a 'crowbar_path' variable set at runtime by
        # the main Gemfile.
        engine_path    = File.join('crowbar_engine', gem_name)
        framework_dir  = File.join(target, 'crowbar_framework')
        routes_dir     = File.join(framework_dir, 'config', 'routes.d')
        gemfile_dir    = File.join(framework_dir, 'Gemfile.d')
        FileUtils.mkdir_p(routes_dir)
        FileUtils.mkdir_p(gemfile_dir)
        routes_plugin  = File.join(routes_dir,  "barclamp-#{@name}-engine.routes")
        gemfile_plugin = File.join(gemfile_dir, "barclamp-#{@name}.gemfile")
        File.open(routes_plugin,  'w') do |f|
          f.puts("mount #{engine_class}, :at => '#{@name}'")
        end unless File.exists?(routes_plugin)
        File.open(gemfile_plugin, 'w') do |f|
          f.puts("gem '#{gem_name}', :path => File.join(crowbar_path, '..', 'barclamps', '#{@name}', '#{engine_path}')")
        end unless File.exists?(gemfile_plugin)
      when 'bin'
        debug("Installing commands for #{@name}")
        FileUtils.mkdir_p(File.join(target,'bin'))
        FileUtils.cp_r(File.join(@source,'bin'),target)
        FileUtils.chmod_R(0755,File.join(target,'bin'))
      when 'etc'
        debug("Installing configs for #{@name}")
        FileUtils.mkdir_p(File.join(@root,'etc'))
        FileUtils.cp_r(File.join(@source,'etc'),@root)
      when 'updates'
        debug("Installing Sledgehammer updates for #{@name}")
        FileUtils.mkdir_p(File.join(@root,'updates'))
        FileUtils.cp_r(File.join(@source,'updates'),@root)
      when 'BDD'
        debug("Installing BDD framework components for #{@name}")
        FileUtils.mkdir_p(File.join(target,'BDD'))
        FileUtils.cp_r(File.join(@source,'BDD'),target)
      when 'doc'
        debug("Installing documentation for #{@name}")
        FileUtils.mkdir_p(File.join(target,'doc',@name))
        FileUtils.cp_r(File.join(@source,'doc','.'),File.join(target,'doc',@name))
      else
        next if @source == dir
        debug("Copying #{ent} into #{dir}")
        FileUtils.mkdir_p(dir)
        FileUtils.cp_r(File.join(@source,ent),dir)
      end
    end
  end

  def install_migrations
    return if skip_migrations
    FileUtils.cd(File.join(target,"crowbar_framework")) do
      debug "#{Dir.pwd}: Installing migrations for #{@name}"
      unless system "su -s /bin/bash -c 'RAILS_ENV=production bundle exec rake railties:install:migrations' crowbar"
        fatal("Installing migrations failed.")
      end
      debug "Making sure database is created #{@name}"
      unless system "su -s /bin/bash -c 'RAILS_ENV=production bundle exec rake db:create' crowbar"
        fatal("Creating database failed.")
      end
      debug "Running migrations for #{@name}"
      unless system "su -s /bin/bash -c 'RAILS_ENV=production bundle exec rake db:migrate' crowbar"
        fatal("Running migrations failed.")
      end
    end
  end

  def run_actions(stage)
    return if skip_install_actions
    setup_dir = File.join(dir,'setup')
    return unless File.directory?(setup_dir)
    FileUtils.cd(setup_dir) do
      Dir.glob("*.#{stage}").sort.each do |action|
        next unless File.executable?(action)
        next if system("CROWBAR_DIR=\"#{target}\" BC_PATH=\"#{dir}\" ./#{action}")
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
        tgt=File.join(tgt,@name)
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
    @installed = true
  end
end

opts = GetoptLong.new(
  [ '--help', '-h', GetoptLong::NO_ARGUMENT ],
  [ '--debug', '-d', GetoptLong::NO_ARGUMENT ],
  [ '--build', GetoptLong::NO_ARGUMENT ],
  [ '--deploy', '-i', GetoptLong::NO_ARGUMENT ],
  [ '--no-files', '-x', GetoptLong::NO_ARGUMENT ],
  [ '--no-engines', '-e', GetoptLong::NO_ARGUMENT ],
  [ '--no-install-actions', '-a', GetoptLong::NO_ARGUMENT ],
  [ '--base-dir', '-b', GetoptLong::REQUIRED_ARGUMENT ],
  [ '--root', '-r', GetoptLong::REQUIRED_ARGUMENT ],
  [ '--test', '-t', GetoptLong::REQUIRED_ARGUMENT ],
  [ '--force', '-f', GetoptLong::NO_ARGUMENT ]
)

def usage()
  puts "Usage:"
  puts "#{__FILE__} [--help] [--debug] [--no-files] [--no-install-actions] [--deploy] [--build] [--base-dir <dir>] [--root <dir>] /path/to/new/barclamp"
  exit
end

class Barclamps < Hash
  attr_accessor :no_rsync, :base_dir, :root, :no_install_actions
  attr_accessor :no_migrations, :no_rsync, :no_files, :no_engines
  attr_accessor :force_install, :tmpdir

  def initialize
    @force_install = false
    @base_dir = "/opt/dell"
    @root = nil
    @no_install_actions = false
    @no_migrations = false
    @no_rsync = false
    @no_files = false
    @no_engines = false
    @tmpdir = "/tmp/bc_install-#{Process.pid}-#{Kernel.rand(65535)}"
    debug "tarball tmpdir: #{@tmpdir}"
    FileUtils.mkdir_p(@tmpdir)
  end

  def rm_tmpdir
    if File.directory?(@tmpdir)
      debug "temporary directory #{@tmpdir} will be removed"
      system "rm -rf #{@tmpdir}"
    end
  end

  def set_bc_opts(bc)
    bc.target = @base_dir
    bc.root = @root if @root
    bc.skip_files = @no_files
    bc.skip_install_actions = @no_install_actions
    bc.skip_migrations = @no_migrations
    bc.skip_engines = @no_engines
  end

  def add_dir(src)
    bc = BarclampFS.new(File.expand_path(src))
    debug("Will install #{bc.name} from #{src}")
    set_bc_opts(bc)
    self[bc.name] = bc
  end

  def add_installed(src)
    bc = BarclampFS.new(File.expand_path(src))
    bc.installed = true
    return if self[bc.name]
    debug("Noting that #{bc.name} is already installed.")
    self[bc.name] = bc
  end

  def add_tarball(tarball)
    # This might be a barclamp tarball.  Expand it into a temporary location.
    src=File.expand_path(tarball)
    system "tar xzf \"#{src}\" -C \"#{tmpdir}\""
    target="#{tmpdir}/#{src.split("/")[-1].split(".")[0]}"
    if File.exists?(File.join(target,"crowbar.yml"))
      add_dir(target)
    else
      puts "#{src} is not a barclamp tarball, ignoring."
    end
  end

  def sanity_check
    values.each do |bc|
      next if bc.installed
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
    end
  end

  def install_bc(bc)
    return true if bc.installed
    bc.requires.each do |prereq|
      next unless self.has_key?(prereq)
      install_bc(self[prereq])
    end
    bc.install
  end

  def install
    values.sort_by{|c| c["crowbar"]["order"].to_i rescue 9999}.each do |bc|
      next if bc.installed
      begin
        debug "installing barclamp #{bc.name}"
        bc.install
      rescue StandardError => e
        if ENV['DEBUG'] == "true"
          debug "temporary directory #{tmpdir} will be left for debugging if it exists"
        else
          rm_tmpdir
        end
        puts e
        puts e.backtrace
        puts "Install of #{bc[:name]} failed."
        exit 3
      end
    end
    rm_tmpdir
  end
end

candidates = Barclamps.new

opts.each do |opt, arg|
  case opt
  when "--help" then usage
  when "--debug" then ENV['DEBUG']="true"; debug "debug mode is enabled"
  when "--build"
    candidates.no_install_actions = true
    candidates.no_migrations = true
    candidates.no_rsync = true
    candidates.no_engines = true
  when "--no-framework-install" then candidates.no_framework = true
  when "--no-install-actions" then candidates.no_install_actions = true
  when "--deploy" then true
  when "--no-files" then candidates.no_files = true; debug "no-files is enabled"
  when "--no-engines" then candidates.no_engines = true; debug "no-engine is enabled"
  when "--base-dir" then candidates.base_dir = arg; debug "base-dir is #{candidates.base_dir}"
  when "--force" then candidates.force_install = true
  when "--root" then candidates.root = arg
  end
end

usage if ARGV.length < 1

ARGV.each do |src|
  bc=nil
  case
  # We were handed a tarball.
  when /tar\.gz|tgz$/ =~ src then candidates.add_tarball(src)
  # We were handed something that looks like a path to a barclamp
  when File.exists?(File.join(src,"crowbar.yml")) then candidates.add_dir(src)
  when File.exists?(File.join(candidates.base_dir,"barclamps",src,"crowbar.yml"))
    candidates.add_dir(File.join(candidates.base_dir,"barclamps",src))
  else
    puts "#{src} is not a barclamp, ignoring."
    next
  end
end

# Load up any previously-installed barclamps from our install target.
# This will help ensure that our dependencies get solved correctly.
Dir.glob(File.join((candidates.root || ''),candidates.base_dir, "barclamps","*")).each do |d|
  next unless File.directory?(d) &&
    File.exists?(File.join(d,"crowbar.yml"))
  candidates.add_installed(d)
end

candidates.sanity_check

candidates.install

exit 0
