# vi:syntax=ruby
#
# Copyright 2011-2013, Dell
# Copyright 2013-2015, SUSE Linux GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

#
# First ensure you have the necessary gems by cd'ing to this directory and
# running 'bundle install'. Then use as follows:
#
#   $ export GUARD_RELEASE_NAME=development # Default `development`
#   $ export GUARD_SYNC_USER=root # Default `root`
#   $ export GUARD_SYNC_HOST=192.168.124.10 # Default `192.168.124.10`
#   $ export GUARD_SYNC_PORT=22 # Default `22`
#   $ export GUARD_TREE_TARGET=/opt/crowbar # Default `/opt/crowbar`
#   $ export GUARD_MIRROR_TARGET=/root/barclamps # Default `/root/barclamps`
#   $ export GUARD_SCRIPT_TARGET=/opt/crowbar/bin # Default `/opt/crowbar/bin`
#   $ bundle exec guard
#
# Now all required files and directories are getting synchronized with the
# admin node so you can work with this crowbar files now. More about Guard
# at https://github.com/guard/guard#readme.
#

def value_for(variable, default)
  target = if ENV[variable]
    ENV[variable]
  else
    default
  end

  if target.empty?
    fail "#{variable} have to be non-empty"
  end

  target
end

user = value_for(
  "GUARD_SYNC_USER",
  "root"
)

host = value_for(
  "GUARD_SYNC_HOST",
  "192.168.124.10"
)

port = value_for(
  "GUARD_SYNC_PORT",
  "22"
)

notification :off

group :tree do
  target = value_for(
   "GUARD_TREE_TARGET",
   "/opt/crowbar"
  )

  Pathname.new("barclamps").children.each do |barclamp|
    next unless barclamp.directory?

    exclude_tree = File.expand_path(
      "../.guard-exclude-tree",
      __FILE__
    )

    File.open(exclude_tree, "w") do |file|
      file.write "+ /BDD/\n"
      file.write "+ /bin/\n"
      file.write "+ /chef/\n"
      file.write "+ /crowbar_framework/\n"
      file.write "+ /doc/\n"
      file.write "+ /etc/\n"
      file.write "+ /updates/\n"
      file.write "- navigation.rb\n"
      file.write "- catalog.yml\n"
      file.write "- ==BC-MODEL==*\n"
      file.write "- *==BC-MODEL==\n"
      file.write "- *==BC-MODEL==*\n"
      file.write "- *.swp\n"
      file.write "- /*\n"
    end

    guard_options = {
      :source => "#{barclamp.to_s}/",
      :destination => target,
      :user => user,
      :remote_address => host,
      :remote_port => port,
      :exclude_from => exclude_tree,
      :sync_on_start => true,
      :ssh => true,
      :cvs_exclude => true
    }

    guard("remote-sync", guard_options) do
      watch(/\A#{barclamp.to_s}\/.+/)
    end

    exclude_barclamp = File.expand_path(
      "../.guard-exclude-barclamp",
      __FILE__
    )

    File.open(exclude_barclamp, "w") do |file|
      file.write "+ /crowbar.yml\n"
      file.write "+ /Gemfile\n"
      file.write "+ /Rakefile\n"
      file.write "+ /README.md\n"
      file.write "+ /doc/\n"
      file.write "+ /smoketest/\n"
      file.write "+ /spec/\n"
      file.write "+ /updates/\n"
      file.write "- *.swp\n"
      file.write "- /*\n"
    end

    config_options = {
      :source => "#{barclamp.to_s}/",
      :destination => File.join(target, barclamp.to_s),
      :user => user,
      :remote_address => host,
      :remote_port => port,
      :exclude_from => exclude_barclamp,
      :exclude_from => nil,
      :include_from => nil,
      :sync_on_start => true,
      :ssh => true,
      :cvs_exclude => true
    }

    guard("remote-sync", config_options) do
      watch(/\A#{barclamp.to_s}\/.+/)
    end
  end
end

group :script do
  target = value_for(
    "GUARD_SCRIPT_TARGET",
    "/opt/crowbar/bin"
  )

  exclude_script = File.expand_path(
    "../.guard-exclude-script",
    __FILE__
  )

  File.open(exclude_script, "w") do |file|
    file.write "+ barclamp_*\n"
    file.write "+ json-*\n"
    file.write "+ install-*\n"
    file.write "+ network-*\n"
    file.write "- *.swp\n"
    file.write "- /*\n"
  end

  script_options = {
    :source => "scripts/",
    :destination => target,
    :user => user,
    :remote_address => host,
    :remote_port => port,
    :exclude_from => exclude_script,
    :sync_on_start => true,
    :ssh => true,
    :cvs_exclude => true
  }

  guard("remote-sync", script_options) do
    watch(/\Ascripts\/barclamp_.+\z/)
    watch(/\Ascripts\/json\-.+\z/)
    watch(/\Ascripts\/install\-.+\z/)
    watch(/\Ascripts\/network\-.+\z/)
  end
end

group :mirror do
  target = value_for(
    "GUARD_MIRROR_TARGET",
    "/root/barclamps"
  )

  exclude_mirror = File.expand_path(
    "../.guard-exclude-mirror",
    __FILE__
  )

  File.open(exclude_mirror, "w") do |file|
    file.write "+ /*\n"
    file.write "- .KEEP_ME\n"
    file.write "- *.swp\n"
  end

  mirror_options = {
    :source => "barclamps/",
    :destination => target,
    :user => user,
    :remote_address => host,
    :remote_port => port,
    :exclude_from => exclude_mirror,
    :sync_on_start => true,
    :ssh => true,
    :cvs_exclude => true
  }

  guard("remote-sync", mirror_options) do
    watch(/\Abarclamps\/.+/)
  end
end
