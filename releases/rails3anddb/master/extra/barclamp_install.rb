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

# the 1st choice is to use the code from the framework since it is most up to date
# however, that code is not always available when installing
require '/opt/dell/bin/barclamp_mgmt_lib.rb'
require 'getoptlong'
require 'pp'

opts = GetoptLong.new(
  [ '--help', '-h', GetoptLong::NO_ARGUMENT ],
  [ '--debug', '-d', GetoptLong::NO_ARGUMENT ],
  [ '--force', '-f', GetoptLong::NO_ARGUMENT ]
)

def usage()
  puts "Usage:"
  puts "#{__FILE__} [--help] [--debug] /path/to/new/barclamp"
  exit
end

force_install = false

opts.each do |opt, arg|
  case opt
    when "--help"
    usage
    when "--debug"
    @@debug = true
    debug "debug mode is enabled"
    when "--force"
    force_install = true
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
  case
  when /tar\.gz|tgz$/ =~ src
    # This might be a barclamp tarball.  Expand it into a temporary location.
    src=File.expand_path(src)
    system "tar xzf \"#{src}\" -C \"#{tmpdir}\""
    target="#{tmpdir}/#{src.split("/")[-1].split(".")[0]}"
    if File.exists?(File.join(target,"crowbar.yml"))
      candidates << target
    else
      puts "#{src} is not a barclamp tarball, ignoring."
    end
  when File.exists?(File.join(src,"crowbar.yml"))
    # We were handed something that looks like a path to a barclamp
    candidates << File.expand_path(src)
  when File.exists?(File.join("/opt","dell","barclamps",src,"crowbar.yml"))
    candidates << File.join("/opt","dell","barclamps",src)
  else
    puts "#{src} is not a barclamp, ignoring."
  end
end

debug "checking candidates: #{candidates.to_s}"

barclamps = Hash.new
candidates.each do |bc|
  # We have already verified that each of the candidates has crowbar.yml
  begin
    debug "trying to parse crowbar.yml"
    barclamp = YAML.load_file File.join(bc,"crowbar.yml")
  rescue
    puts "Exception occured while parsing crowbar.yml in #{bc}, skiping"
    next
  end
  
  if barclamp["barclamp"] and barclamp["barclamp"]["name"]
    name = barclamp["barclamp"]["name"]
  else
    puts "Barclamp at #{bc} has no name, skipping"
    next
  end
  order = 9999
  if barclamp["crowbar"] and barclamp["crowbar"]["order"] and \
    barclamp["crowbar"]["order"].to_i 
    order = barclamp["crowbar"]["order"].to_i
  end
  barclamps[name] = { :src => bc, :name => name, :order => order, :yaml => barclamp }
  debug "barclamp[#{name}] = #{barclamps[name].pretty_inspect}"
end

debug "installing barclamps:"
barclamps.values.sort_by{|v| v[:order]}.each do |bc|
  debug "bc = #{bc.pretty_inspect}"
  begin
    unless /^\/opt\/dell\/barclamps\// =~ bc[:src]
      target="/opt/dell/barclamps/#{bc[:src].split("/")[-1]}"
      if File.directory? target
        debug "target directory #{target} exists"
        if File.exists? "#{target}/crowbar.yml"
          debug "#{target}/crowbar.yml file exists"
          if File.exists? "#{target}/sha1sums"
            debug "#{target}/sha1sums file exists"
            unless force_install or system "cd \"#{target}\"; sha1sum --status -c sha1sums"
              debug "force_install mode is disabled and not all file checksums do match"
              puts "Refusing to install over non-pristine target #{target}"
              puts "Please back up the following files:"
              system "cd \"#{target}\"; sha1sum -c sha1sums |grep -v OK"
              puts "and rerun the install after recreating the checksum file with:"
              puts "  cd \"#{target}\"; find -type f -not -name sha1sums -print0 | \\"
              puts "       xargs -0 sha1sum -b >sha1sums"
              puts "(or use the --force switch)"
              debug "temporary directory #{tmpdir} will be removed if it exists"
              system "rm -rf #{tmpdir}" if File.directory?(tmpdir)
              exit -1
            end
          elsif not force_install
            debug "force_install mode is disabled and #{target}/sha1sums file does not exist"
            puts "#{target} already exists, but it does not have checksums."
            puts "Please back up any local changes you may have made, and then"
            puts "create a checksums file with:"
            puts "  cd \"#{target}\"; find -type f -not -name sha1sums -print0 | \\"
            puts "       xargs -0 sha1sum -b >sha1sums"
            puts "(or use the --force switch)"
            debug "temporary directory #{tmpdir} will be removed if it exists"
            system "rm -rf #{tmpdir}" if File.directory?(tmpdir)
            exit -1
          end
        else
          debug "#{target}/crowbar.yml does not exist"
          puts "#{target} exists, but it is not a barclamp."
          puts "Cowardly refusing to overwrite it."
          debug "temporary directory #{tmpdir} will be removed if it exists"
          system "rm -rf #{tmpdir}" if File.directory?(tmpdir)
          exit -1
        end
      else
        debug "target directory \"#{target}\" does not exist"
        debug "creating directory \"#{target}\""
        system "mkdir -p \"#{target}\""
      end
      # Only rsync over the changes if this is a different install
      # from the POV of the sha1sums files
      unless File.exists?("#{bc[:src]}/sha1sums") and \
        File.exists?("#{target}/sha1sums") and \
        system "/bin/bash -c 'diff -q <(sort \"#{bc[:src]}/sha1sums\") <(sort \"#{target}/sha1sums\")'"
        debug "syncing \"#{bc[:src]}\" directory and \"#{target}\" directory"
        system "rsync -r \"#{bc[:src]}/\" \"#{target}\""
      end
      bc[:src] = target
    end
    debug "installing barclamp"
    begin
      bc_install bc[:name], bc[:src], bc[:yaml]
    rescue StandardError => e
      debug "exception occurred while installing barclamp"
      raise e
    end
  rescue StandardError => e
    if @@debug
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
