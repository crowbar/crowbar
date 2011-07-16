#!/usr/bin/ruby

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