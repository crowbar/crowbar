#!/usr/bin/ruby

require 'rubygems'
require 'ipaddr'
require 'json'

if ARGV.length != 1
   puts "Invalid call to script."
   exit 1
end

ipv4_arg = ARGV[0]

databag = JSON.load($stdin)
admin = databag['attributes']['network']['networks']['admin']['ranges']['admin']

begin
    ipv4 = IPAddr.new(ipv4_arg)
    ip_start = IPAddr.new(admin['start'])
    ip_end = IPAddr.new(admin['end'])
rescue Exception => e
    puts e
    exit 1
end

if ipv4 < ip_start || ipv4 > ip_end
   puts "admin range is configured from #{admin['start']} to #{admin['end']}"
   exit 1
end
