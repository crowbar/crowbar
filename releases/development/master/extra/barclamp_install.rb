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
require File.join(File.dirname(__FILE__), 'barclamp_mgmt_lib.rb')
require 'getoptlong'
require 'pp'

@@no_rsync = false

opts = GetoptLong.new(
  [ '--help', '-h', GetoptLong::NO_ARGUMENT ],
  [ '--debug', '-d', GetoptLong::NO_ARGUMENT ],
  [ '--build', GetoptLong::NO_ARGUMENT ],
  [ '--deploy', '-i', GetoptLong::NO_ARGUMENT ],
  [ '--no-files', '-x', GetoptLong::NO_ARGUMENT ],
  [ '--no-install-actions', '-a', GetoptLong::NO_ARGUMENT ],
  [ '--no-chef', '-c', GetoptLong::NO_ARGUMENT ],
  [ '--base-dir', '-b', GetoptLong::REQUIRED_ARGUMENT ],
  [ '--root', '-r', GetoptLong::REQUIRED_ARGUMENT ],
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

opts.each do |opt, arg|
  case opt
  when "--help" then usage
  when "--debug" then ENV['DEBUG']="true"; debug "debug mode is enabled"
  when "--build"
    @@no_install_actions = true
    @@no_chef = true
    @@no_migrations = true
    @@no_rsync = true
  when "--no-framework-install" then @@no_framework = true
  when "--no-install-actions" then @@no_install_actions = true
  when "--deploy" then @@deploy = true
  when "--no-files" then @@no_files = true; debug "no-files is enabled"
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
