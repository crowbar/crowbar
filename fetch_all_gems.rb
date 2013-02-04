#!/usr/bin/ruby
require 'rubygems'
require 'rubygems/gem_runner'
require 'rubygems/exceptions'
# In theory this shoulc have already been required.
require 'rubygems/version_option'

# Stupid hack to allow multiple --version options.
module Gem::VersionOption
  def add_version_option(task = command, *wrap)
    OptionParser.accept Gem::Requirement do |value|
      Gem::Requirement.new value
    end
    add_option('-v', '--version VERSION', Gem::Requirement,
               "Specify version of gem to #{task}", *wrap) do
      |value, options|
      options[:version] = Array.new unless options[:version].kind_of?(Array)
      options[:version] << value
      options[:prerelease] = true if value.prerelease?
    end
  end
end

args = ARGV.clone
begin
  Gem::GemRunner.new.run args
rescue Exception => e
  e.backtrace
end
