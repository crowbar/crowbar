#!/usr/bin/ruby

require 'rubygems'
require 'json'

if ARGV.length != 1
   puts "Invalid call to script."
   exit 1
end

ipv4_arg = ARGV[0]

databag = JSON.load($stdin)
admin = databag['attributes']['network']['networks']['admin']['ranges']['admin']

def convert_ip(ip)
    begin
        r = ip.split('.').collect!{|n| n.to_i() if Integer(n)}
    rescue ArgumentError => e
        raise Exception.new("#{ip} is not a valid IP address.")
    end
    if r.length != 4
       raise Exception.new("#{ip} is not a valid IP address.")
    end
    r
end

begin
    ipv4 = convert_ip(ipv4_arg)
    ip_start = convert_ip(admin['start'])
    ip_end = convert_ip(admin['end'])
rescue Exception => e
    puts e
    exit 1
end

for i in 0..3 do
    if ipv4[i] < ip_start[i] or ipv4[i] > ip_end[i]
        exit 1
    end
end
