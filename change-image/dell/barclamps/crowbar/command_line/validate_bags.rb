#!/usr/bin/ruby
# Copyright 2011, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#  http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#

require File.join(File.dirname(__FILE__), 'validate_data_bag' )

def verify_bags(base_dir)
  err = false  
  Dir.chdir(base_dir) { |d|
    Dir["*/*.json"].each { |bag|
        schema = find_schema_for_file(bag)
        puts "validating #{bag} against #{schema}"
        begin
          rc = validate(schema,bag)
        rescue Exception => e
          puts "Error: validating #{bag} against #{schema}"
          puts "Error: #{e.message}"
          rc = -1 
        end
        err = true if rc != 0
    }
  }
  err
end


def find_schema_for_file(f)
  dir = File.dirname(f)
  base = File.basename(f,'.json')
  components = base.split("-")
  cnt = components.length
  # trim sections of file name (-) to try to find schema
  check_name =""
  begin
    check_name = "#{dir}/#{components[0..cnt].join('-')}.schema"
    break if File.exists?(check_name)    
    cnt = cnt - 1
  end while cnt >0
  check_name
end



if __FILE__ == $0
  base_dir = ARGV[0].nil? ? "/opt/dell/chef/data_bags" : ARGV[0]
  puts "Using #{base_dir}"
  err = verify_bags( base_dir)
  exit -3 if err
  exit 0
end
