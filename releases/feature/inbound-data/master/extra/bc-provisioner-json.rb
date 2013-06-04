#!/usr/bin/env ruby
# import pre-existing ssh-keys into provisioner template

require 'rubygems'
require 'json'

keys = File.read('/root/.ssh/authorized_keys')

databag = JSON.load($stdin)
databag['attributes']['provisioner']['access_keys'] += keys;

puts JSON.pretty_generate databag
