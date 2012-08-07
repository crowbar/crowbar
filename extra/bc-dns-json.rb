#!/usr/bin/env ruby

require 'rubygems'
require 'json'

domain, *forwarders = ARGV

#json = File.read(ARGV[0])
#databag = JSON.parse(json)

databag = JSON.load($stdin)
databag['attributes']['dns']['domain']     = domain
databag['attributes']['dns']['forwarders'] = forwarders

puts JSON.pretty_generate databag
