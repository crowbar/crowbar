#!/usr/bin/env ruby
# import pre-existing ssh-keys into provisioner template

require 'rubygems'
require 'json'

keys = File.read('/root/.ssh/authorized_keys') rescue ""

databag = JSON.load($stdin)
if keys != ""
  databag['attributes']['provisioner']['access_keys'] += "\n" + keys;
end

puts JSON.pretty_generate databag
