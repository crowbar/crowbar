#!/usr/bin/env ruby
require 'yaml'

# This is very cheesy, and should only be called by the build script.
tree = YAML.load_file ARGV[0]
ARGV[1..-1].each do |n|
  tree = tree[n]
end
tree.each do |n|
  puts n
end
