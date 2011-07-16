#!/usr/bin/ruby

require 'kwalify'
require 'uri'

require '/opt/dell/openstack_manager/app/models/crowbar_validator.rb'

def validate(schema_filename, config_filename)
  ## validate schema definition
  metavalidator = Kwalify::MetaValidator.instance
  parser = Kwalify::Yaml::Parser.new(metavalidator)
  document = parser.parse_file(schema_filename)

  rc = 0

  # show errors
  for e in parser.errors
    rc = -1
    puts "#{schema_filename}: #{e.linenum}:#{e.column} [#{e.path}] #{e.message}"
  end if parser.errors && !parser.errors.empty?

  ## Validate the config file
  validator = CrowbarValidator.new(schema_filename)
  parser = Kwalify::Yaml::Parser.new(validator)
  document = parser.parse_file(config_filename)

  ## show errors
  for e in parser.errors
    rc = -2 if rc == 0 
    puts "#{config_filename}: #{e.linenum}:#{e.column} [#{e.path}] #{e.message}"
  end if parser.errors && !parser.errors.empty?

  rc
end

if __FILE__ == $0

if ARGV.size != 2
  puts "Must specify two files\n"
  puts "Usage: validate_data_bag <schema file> <data file>\n"
  exit -3
end

if !File.exists?(ARGV[0])
  puts "#{ARGV[0]} must exist\n"
  puts "Usage: validate_data_bag <schema file> <data file>\n"
  exit -4
end

if !File.exists?(ARGV[1])
  puts "#{ARGV[1]} must exist\n"
  puts "Usage: validate_data_bag <schema file> <data file>\n"
  exit -5
end

rc = -1
begin
  rc = validate(ARGV[0], ARGV[1])
rescue Exception => e
  puts "Failed with exception: #{e.message}"
end

exit rc

end