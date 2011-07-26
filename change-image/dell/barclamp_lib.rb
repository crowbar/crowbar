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
# See the License for the specific language governing permissions and
# limitations under the License.
# 

require 'rubygems'
require 'net/http'
require 'net/http/digest_auth'
require 'uri'
require 'json'
require 'getoptlong'

@debug = false
@hostname = ENV["CROWBAR_IP"] 
# DO NOT CHANGE THE NEXT 2 LINES
# gather_cli replies on the exact format they are in. 
@hostname = "127.0.0.1" unless @hostname
@port = 3000
@headers = {
  "Accept" => "application/json",
  "Content-Type" => "application/json"
}
@data = ""
@allow_zero_args = false
@timeout = 500
@key = ENV["CROWBAR_KEY"]
if @key
  @username=@key.split(':',2)[0]
  @password=@key.split(':',2)[1]
end

#
# Parsing options can be added by adding to this list before calling opt_parse
# 
@options = [
    [ [ '--help', '-h', GetoptLong::NO_ARGUMENT ], "--help or -h - help" ],
    [ [ '--username', '-U', GetoptLong::REQUIRED_ARGUMENT ], "--username <username> or -U <username>  - specifies the username" ],
    [ [ '--password', '-P', GetoptLong::REQUIRED_ARGUMENT ], "--password <password> or -P <password>  - specifies the password" ],
    [ [ '--hostname', '-n', GetoptLong::REQUIRED_ARGUMENT ], "--hostname <name or ip> or -n <name or ip>  - specifies the destination server" ],
    [ [ '--port', '-p', GetoptLong::REQUIRED_ARGUMENT ], "--port <port> or -p <port> - specifies the destination server port" ],
    [ [ '--debug', '-d', GetoptLong::NO_ARGUMENT ], "--debug or -d - turns on debugging information" ],
    [ [ '--data', GetoptLong::REQUIRED_ARGUMENT ], "--data <data> - used by create or edit as data (must be in json format)" ],
    [ [ '--file', GetoptLong::REQUIRED_ARGUMENT ], "--file <file> - used by create or edit as data when read from a file (must be in json format)" ],
    [ [ '--timeout', GetoptLong::REQUIRED_ARGUMENT ], "--timeout <seconds> - timeout in seconds for read http reads" ]
]

#
# New commands can be added by adding to this list before calling run_command
#
# Proposal is an example of running sub-commands
#
@proposal_commands = {
  "list" => [ "proposal_list", "list - show a list of current proposals" ],
  "create" => [ "proposal_create ARGV.shift", "create <name> - create a proposal" ],
  "show" => [ "proposal_show ARGV.shift", "show <name> - show a specific proposal" ],
  "edit" => [ "proposal_edit ARGV.shift", "edit <name> - edit a new proposal" ],
  "delete" => [ "proposal_delete ARGV.shift", "delete <name> - delete a proposal" ],
  "commit" => [ "proposal_commit ARGV.shift", "commit <name> - Commit a proposal to active" ],
  "dequeue" => [ "proposal_dequeue ARGV.shift", "dequeue <name> - Dequeue a proposal to active" ]
}

@commands = {
  "help" => [ "help", "help - this page" ],
  "api_help" => [ "api_help", "crowbar API help - help for this barclamp." ],
  "list" => [ "list", "list - show a list of current configs" ],
  "show" => [ "show ARGV.shift", "show <name> - show a specific config" ],
  "delete" => [ "delete ARGV.shift", "delete <name> - delete a config" ],
  "proposal" => [ "run_sub_command(@proposal_commands, ARGV.shift)", "proposal - Proposal sub-commands", @proposal_commands ],
  "elements" => [ "elements", "elements - List elements of a #{@barclamp} deploy" ],
  "element_node" => [ "element_node ARGV.shift", "element_node <name> - List nodes that could be that element" ],
  "transition" => [ "transition(ARGV.shift,ARGV.shift)", "transition <name> <state> - Transition machine named name to state" ]
}


def print_commands(cmds, spacer = "  ")
  cmds.each do |key, command|
    puts "#{spacer}#{command[1]}"
    print_commands(command[2], "  #{spacer}") if command[0] =~ /run_sub_command\(/
  end
end

def usage (rc)
  puts "Usage: crowbar #{@barclamp} [options] <subcommands>"
  @options.each do |options|
    puts "  #{options[1]}"
  end
  print_commands(@commands)
  exit rc
end

def help
  usage 0
end

def authenticate(req,uri,data=nil)
  uri.user=@username
  uri.password=@password
  res=nil
  Net::HTTP.start(uri.host, uri.port) {|http|
    http.read_timeout = @timeout
    r = req.new(uri.request_uri,@headers)
    r.body = data if data
    res = http.request r
    puts "DEBUG: (a) return code: #{res.code}" if @debug
    puts "DEBUG: (a) return body: #{res.body}" if @debug
    puts "DEBUG: (a) return headers: #{res.headers}" if @debug
    if res['www-authenticate']
      digest_auth=Net::HTTP::DigestAuth.new
      auth=Net::HTTP::DigestAuth.new.auth_header(uri,
                                                 res['www-authenticate'],
                                                 req::METHOD)
      r.add_field 'Authorization', auth
      res = http.request r
    end
  }
  res
end  

def get_json(path)
  uri = URI.parse("http://#{@hostname}:#{@port}/crowbar/#{@barclamp}/1.0#{path}")
  res = authenticate(Net::HTTP::Get,uri)

  puts "DEBUG: (g) hostname: #{uri.host}:#{uri.port}" if @debug
  puts "DEBUG: (g) request: #{uri.path}" if @debug
  puts "DEBUG: (g) return code: #{res.code}" if @debug
  puts "DEBUG: (g) return body: #{res.body}" if @debug

  return [res.body, res.code.to_i ] if res.code.to_i != 200

  struct = JSON.parse(res.body)

  puts "DEBUG: (g) JSON parse structure = #{struct.inspect}" if @debug

  return [struct, 200]
end

def post_json(path, data)
  uri = URI.parse("http://#{@hostname}:#{@port}/crowbar/#{@barclamp}/1.0#{path}")
  res = authenticate(Net::HTTP::Post,uri,data)

  puts "DEBUG: (post) hostname: #{uri.host}:#{uri.port}" if @debug
  puts "DEBUG: (post) request: #{uri.path}" if @debug
  puts "DEBUG: (post) data: #{@data}" if @debug
  puts "DEBUG: (post) return code: #{res.code}" if @debug
  puts "DEBUG: (post) return body: #{res.body}" if @debug

  [res.body, res.code.to_i ]
end

def put_json(path, data)
  uri = URI.parse("http://#{@hostname}:#{@port}/crowbar/#{@barclamp}/1.0#{path}")
  res = authenticate(Net::HTTP::Put,uri,data)

  puts "DEBUG: (put) hostname: #{uri.host}:#{uri.port}" if @debug
  puts "DEBUG: (put) request: #{uri.path}" if @debug
  puts "DEBUG: (put) data: #{@data}" if @debug
  puts "DEBUG: (put) return code: #{res.code}" if @debug
  puts "DEBUG: (put) return body: #{res.body}" if @debug

  [res.body, res.code.to_i ]
end

def delete_json(path)
  uri = URI.parse("http://#{@hostname}:#{@port}/crowbar/#{@barclamp}/1.0#{path}")
  res = authenticate(Net::HTTP::Delete,uri)

  puts "DEBUG: (d) hostname: #{uri.host}:#{uri.port}" if @debug
  puts "DEBUG: (d) request: #{uri.path}" if @debug
  puts "DEBUG: (d) return code: #{res.code}" if @debug
  puts "DEBUG: (d) return body: #{res.body}" if @debug

  [res.body, res.code.to_i ]
end


def list
  struct = get_json("/")

  if struct[1] != 200
    [ "Failed to talk to service list: #{struct[1]}: #{struct[0]}", 1 ]
  elsif struct[0].nil? or struct[0].empty? 
    [ "No current configurations", 0 ]
  else
    out = ""
    struct[0].each do |name|
      out = out + "\n" if out != ""
      out = out + "#{name}"
    end
    [ out, 0 ]
  end
end

def api_help
  struct=get_json("/help")
  if struct[1] != 200
    [ "Failed to talk to service list: #{struct[1]}: #{struct[0]}", 1 ]
  elsif struct[0].nil? or struct[0].empty? 
    [ "No help", 0 ]
  else
    [ jj(struct[0]), 0 ]
  end
end

def show(name)
  usage -1 if name.nil? or name == ""

  struct = get_json("/#{name}")

  if struct[1] == 200
    [ "#{JSON.pretty_generate(struct[0])}", 0 ]
  elsif struct[1] == 404
    [ "No current configuration for #{name}", 1 ]
  else
    [ "Failed to talk to service show: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def delete(name)
  usage -1 if name.nil? or name == ""
 
  struct = delete_json("/#{name}")

  if struct[1] == 200
    [ "Deleted #{name}", 0 ]
  elsif struct[1] == 404
    [ "Delete failed for #{name}: Not Found", 1 ] 
  else
    [ "Failed to talk to service delete: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def proposal_list
  struct = get_json("/proposals/")

  if struct[1] != 200
    [ "Failed to talk to service proposal list: #{struct[1]}: #{struct[0]}", 1 ]
  elsif struct[0].nil? or struct[0].empty? 
    [ "No current proposals", 0 ]
  else
    out = ""
    struct[0].each do |name|
      out = out + "\n" if out != ""
      out = out + "#{name}"
    end
    [ out, 0 ]
  end
end

def proposal_show(name)
  usage -1 if name.nil? or name == ""

  struct = get_json("/proposals/#{name}")

  if struct[1] == 200
    [ "#{JSON.pretty_generate(struct[0])}", 0 ]
  elsif struct[1] == 404
    [ "No current proposal for #{name}", 1 ]
  else
    [ "Failed to talk to service proposal show: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def proposal_create(name)
  usage -1 if name.nil? or name == ""

  @data = "{\"id\":\"#{name}\"}" if @data.nil? or @data == ""

  struct = put_json("/proposals", @data)

  if struct[1] == 200
    [ "Created #{name}", 0 ]
  else
    [ "Failed to talk to service proposal create: #{struct[1]}: #{struct[0]}", 1]
  end
end

def proposal_edit(name)
  usage -1 if name.nil? or name == ""

  struct = post_json("/proposals/#{name}", @data)

  if struct[1] == 200
    [ "Edited #{name}", 0 ]
  elsif struct[1] == 404
    [ "Failed to edit: #{name} : Not Found", 1 ]
  elsif struct[1] == 400
    [ "Failed to edit: #{name} : Errors in data\n#{struct[0]}", 1 ]
  else
    [ "Failed to talk to service proposal edit: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def proposal_delete(name)
  usage -1 if name.nil? or name == ""

  struct = delete_json("/proposals/#{name}")

  if struct[1] == 200
    [ "Deleted #{name}", 0 ]
  elsif struct[1] == 404
    [ "Delete failed for #{name}: Not Found", 1 ] 
  else
    [ "Failed to talk to service delete: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def proposal_commit(name)
  usage -1 if name.nil? or name == ""

  struct = post_json("/proposals/commit/#{name}", @data)

  if struct[1] == 200
    [ "Committed #{name}", 0 ]
  elsif struct[1] == 202
    [ "Queued #{name} because #{struct[0]}", 0 ]
  else
    [ "Failed to talk to service proposal commit: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def proposal_dequeue(name)
  usage -1 if name.nil? or name == ""

  struct = post_json("/proposals/dequeue/#{name}", @data)

  if struct[1] == 200
    [ "Dequeued #{name}", 0 ]
  else
    [ "Failed to talk to service proposal dequeue: #{struct[1]}: #{struct[0]}", 1 ]
  end
end

def elements
  struct = get_json("/elements")

  if struct[1] != 200
    [ "Failed to talk to service elements: #{struct[1]}: #{struct[0]}", 1 ]
  elsif struct[0].nil? or struct[0].empty?
    [ "No current elements", 1 ]
  else
    out = ""
    struct[0].each do |name|
      out = out + "\n" if out != ""
      out = out + "#{name}"
    end
    [ out, 0 ]
  end
end

def element_node(element)
  usage -1 if element.nil? or element == ""

  struct = get_json("/elements/#{element}")

  if struct[1] != 200
    [ "Failed to talk to service element_node: #{struct[1]}: #{struct[0]}", 1 ]
  elsif struct[0].nil? or struct[0].empty?
    [ "No nodes for #{element}", 1 ]
  else
    out = ""
    struct[0].each do |name|
      out = out + "\n" if out != ""
      out = out + "#{name}"
    end
    [ out, 0 ]
  end
end

def transition(name, state)
  usage -1 if name.nil? or name == ""

  data = {
    "name" => name,
    "state" => state
  }
  struct = post_json("/transition/default", data.to_json)

  if struct[1] == 200
    [ "Transitioned #{name}", 0 ]
  else
    [ "Failed to talk to service transition: #{struct[1]}: #{struct[0]}", 1 ]
  end
end



### Start MAIN ###

def opt_parse()
  sub_options = @options.map { |x| x[0] }
  lsub_options = @options.map { |x| [ x[0][0], x[2] ] }
  opts = GetoptLong.new(*sub_options)

  opts.each do |opt, arg|
    case opt
      when '--help'
        usage 0
      when '--debug'
        @debug = true
      when '--hostname'
        @hostname = arg
      when '--username'
        @username = arg
      when '--password'
        @password = arg
      when '--port'
        @port = arg.to_i
      when '--data'
        @data = arg
      when '--timeout'
        @timeout = arg
      when '--file'
        @data = File.read(arg)
      else
        found = false
        lsub_options.each do |x|
          next if x[0] != opt
          eval x[1]
          found = true
        end
        usage -1 unless found
    end
  end

  if ARGV.length == 0 and !@allow_zero_args
    usage -1
  end

  STDERR.puts "CROWBAR_KEY not set, will not be able to authenticate!" if @username.nil? or @password.nil?
  STDERR.puts "Please set CROWBAR_KEY or use -U and -P" if @username.nil? or @password.nil?
end

def run_sub_command(cmds, subcmd)
  cmd = cmds[subcmd]
  usage -2 if cmd.nil?
  eval cmd[0]
end

def run_command()
  run_sub_command(@commands, ARGV.shift)
end

def main()
  opt_parse
  res = run_command
  puts res[0]
  exit res[1]
end
