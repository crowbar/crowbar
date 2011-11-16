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

tmpdir="/tmp/bc_install-#{Process.pid}-#{Kernel.rand(65535)}"
Dir.mkdir(tmpdir)
candidates = Array.new
ARGV.each do |src|
  case
  when /tar\.gz$/ =~ src
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

barclamps = Hash.new
candidates.each do |bc|
  # We have already verified that each of the candidates has crowbar.yml
  barclamp = YAML.load_file File.join(bc,"crowbar.yml")
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
end

barclamps.values.sort_by{|v| v[:order]}.each do |bc|
  begin
    unless /^\/opt\/dell\/barclamps\// =~ bc[:src]
      system "rsync -r \"#{bc[:src]}/\" \"/opt/dell/barclamps/#{bc[:name]}\""
      bc[:src] = "/opt/dell/barclamps/#{bc[:name]}"
    end
    bc_install bc[:name],bc[:src],bc[:yaml]
  rescue
    puts "Install of #{bc[:name]} failed."
    system "rm -rf #{tmpdir}" if File.directory?(tmpdir)
    exit -3
  end
end
system "rm -rf #{tmpdir}" if File.directory?(tmpdir)

exit 0
