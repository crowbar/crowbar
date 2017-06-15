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

namespace :crowbar do
  require "git"
  require "octokit"
  require "netrc"
  require "yaml"
  require "deep_merge"

  task :configure do
    @git = Git::Lib.new

    config_path = File.expand_path("../config/config.yml", __FILE__)

    config = if File.exist? config_path
      YAML.load_file(
        config_path
      )
    else
      {}
    end

    barclamps_general_path = File.expand_path("../config/barclamps.yml", __FILE__)

    barclamps_general = if File.exist? barclamps_general_path
      YAML.load_file(
        barclamps_general_path
      )
    else
      {}
    end

    barclamps_local_path = File.expand_path("../config/barclamps.local.yml", __FILE__)

    barclamps_local = if File.exist? barclamps_local_path
      YAML.load_file(
        barclamps_local_path
      )
    else
      {}
    end

    @barclamps = if config["local_only"]
      barclamps_local
    else
      barclamps_general + barclamps_local
    end
  end

  desc "Help"
  task :help do
    config = {
      local_only: "(true|false) only use config/barclamps.local.yml"
    }
    hints = [
      "Create config/config.yml for general configuration",
      "Override config/barclamps.yml with config/barclamps.local.yml"
    ]

    puts "Hints:"
    hints.each do |hint|
      puts "  #{hint}"
    end

    puts "Configuration Options: (config/config.yml)"
    config.each do |option, helptext|
      puts "  #{option} = #{helptext}"
    end
  end

  desc "Init all barclamps"
  task init: [:fork, :clone, :add_upstream, :add_susecloud] do
    # nothing to do here
  end

  desc "Update all clones"
  task update: [:pull_upstream] do
    # nothing to do here
  end

  desc "Forks all required barclamps to your home project"
  task fork: [:configure] do
    client = Octokit::Client.new(netrc: true)

    @barclamps.each do |barclamp|
      begin
        if client.repository? "#{client.login}/#{barclamp}"
          puts "Barclamp #{barclamp} already forked. Skipping..."
        else
          puts "Forking barclamp #{barclamp}..."
          client.fork("crowbar/#{barclamp}")
        end
      rescue Octokit::NotFound
        puts "Barclamp #{barclamp} does not exist"
      end
    end
  end

  desc "Clones all required barclamps into the barclamps folder"
  task clone: [:configure] do
    client = Octokit::Client.new(netrc: true)

    Dir.chdir "barclamps" do
      @barclamps.each do |barclamp|
        short_name = barclamp.gsub(/\Acrowbar-/, "")
        if Dir.exist?(barclamp) || Dir.exist?(short_name)
          puts "Barclamp #{barclamp} already exists. Skipping..."
        else
          if client.repository? "#{client.login}/#{barclamp}"
            puts "Cloning barclamp #{barclamp}..."
            Git.clone("git@github.com:#{client.login}/#{barclamp}.git", short_name)
          else
            puts "Barclamp #{barclamp} does not exist"
          end
        end
      end
    end
  end

  desc "Add upstream remote to the crowbar organisation"
  task add_upstream: [:configure] do
    client = Octokit::Client.new(netrc: true)

    @barclamps.each do |barclamp|
      barclamp_dir = File.join("barclamps", barclamp.gsub(/\Acrowbar-/, ""))

      unless Dir.exist? barclamp_dir
        puts "Barclamp #{barclamp} directory does not exist"
        next
      end

      Dir.chdir barclamp_dir do
        next if @git.remotes.include? "upstream"

        if client.repository? "crowbar/#{barclamp}"
          puts "Adding remote upstream for #{barclamp}..."
          @git.remote_add("upstream", "git@github.com:crowbar/#{barclamp}.git", fetch: true)
        else
          puts "Barclamp #{barclamp} doesn't exist within Crowbar org"
        end
      end
    end
  end

  desc "Add upstream remote to the SUSE-Cloud organisation"
  task add_susecloud: [:configure] do
    client = Octokit::Client.new(netrc: true)

    @barclamps.each do |barclamp|
      barclamp_dir = File.join("barclamps", barclamp.gsub(/\Acrowbar-/, ""))

      unless Dir.exist? barclamp_dir
        puts "Barclamp #{barclamp} directory does not exist"
        next
      end

      Dir.chdir barclamp_dir do
        next if @git.remotes.include? "suse-cloud"

        if client.repository? "SUSE-Cloud/#{barclamp}"
          puts "Adding remote suse-cloud for #{barclamp}..."
          @git.remote_add("suse-cloud", "git@github.com:SUSE-Cloud/#{barclamp}.git", fetch: true)
        else
          puts "Barclamp #{barclamp} doesn't exist within SUSE-Cloud org"
        end
      end
    end
  end

  desc "Update remotes repositories"
  task update_remotes: [:configure] do
    @barclamps.each do |barclamp|
      barclamp_dir = File.join("barclamps", barclamp.gsub(/\Acrowbar-/, ""))

      unless Dir.exist? barclamp_dir
        puts "Barclamp #{barclamp} directory does not exist"
        next
      end

      Dir.chdir barclamp_dir do
        ["upstream", "suse-cloud"].each do |remote|
          if @git.remotes.include? remote
            puts "Updating #{remote} of #{barclamp}..."
            msg = @git.fetch(remote, prune: true)
            puts msg unless msg.empty?
          else
            puts "Remote #{remote} of #{barclamp} does not exist"
          end
        end
      end
    end
  end

  desc "Pull from upstream/master"
  task pull_upstream: [:configure] do
    @barclamps.each do |barclamp|
      barclamp_dir = File.join("barclamps", barclamp.gsub(/\Acrowbar-/, ""))

      unless Dir.exist? barclamp_dir
        puts "Barclamp #{barclamp} directory does not exist"
        next
      end

      Dir.chdir barclamp_dir do
        current_branch = @git.branch_current
        stash = { saved: false, branch: current_branch }

        # save a stash if there are changes
        unless @git.diff_files.empty?
          puts "Saving stash of #{barclamp}..."
          @git.stash_save("Automatic stash by rake - #{Time.now}")
          stash[:saved] = true
        end

        # checkout master if on a different branch
        unless current_branch == "master"
          puts "Checking out master of #{barclamp}..."
          @git.checkout("master")
        end

        # pull changes from upstream/master
        if @git.remotes.include? "upstream"
          puts "Pull from upstream/master of #{barclamp}"
          msg = @git.pull("upstream")
          unless msg.include? "Already up-to-date"
            puts msg
            puts "^^^^^^^^^^^^^^^^^^^^^^^^^^^"
          end
        else
          puts "Remote upstream of #{barclamp} does not exist"
        end

        # reapply stash
        if stash[:saved]
          puts "Reapplying stash to #{stash[:branch]}..."
          @git.checkout(stash[:branch]) unless stash[:branch] == "master"
          @git.stash_apply
          @git.stash_clear
        end
      end
    end
  end
end

namespace :test do
  task dependencies: [] do
    system("sudo pip install bashate") unless ENV["TRAVIS"]
  end

  desc "Run bashate tests"
  task bashate: [:dependencies] do
    ["scripts/install-chef-suse.sh",
     "sledgehammer/start-up.sh"].each do |script|
      system("bash -n #{script}")
      if $?.exitstatus != 0
        exit 3
      end

      system("bashate --ignore E006,E010,E011,E020 #{script}")
      if $?.exitstatus != 0
        exit 4
      end

      # checking for tabs in the file
      if File.open(script).grep(/\t/).any?
        exit 5
      end
    end
  end
end

task default: [
  "test:bashate"
]
