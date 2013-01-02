#!/usr/bin/env ruby
require 'yaml'

# This is very cheesy, and should only be called by the build script.
tree = YAML.load_file ARGV[0]
ARGV[1..-1].each do |n|
  tree = tree[n] unless tree.nil?
end
case tree.class
when Array
  tree.each do |n|
    puts n unless n.nil?
  end
when Hash
  tree.each do |k,v|
    puts "#{k}, #{v}" unless n.nil?
  end
else
  puts tree unless tree.nil?
end
