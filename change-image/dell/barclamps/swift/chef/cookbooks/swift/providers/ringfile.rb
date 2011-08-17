#
# Copyright 2011, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Author: andi abes
#

##
# This LWRP will read the current state of a current ring, by executing 
# swift-ring-builder and parsing its output. It would then compare the 
# desired set of disks to the disks present.
# It currently does not change parameters (zone assignment or weight).
# to achieve that, you'd have to remove and readd the disk.

##
# some internal data structs to hold ring info read from existing files
class RingInfo
  attr_accessor :partitions, :replicas, :zones, :device_num, :devices, :min_part_hours
  
  class RingDeviceInfo
    attr_accessor :id, :zone, :ip, :port, :name, :weight, :partitions
    
    def initialize
      Chef::Log.debug "new device"
      self
    end
    def to_s
      s = "" 
      s <<"@" << @id <<":" << @zone << "[" << @ip <<":" << @port <<"]/" << @name 
    end
  end
  
  def initialize
    @devices = {}
    self
  end
  
  def self.dev_key ip,port,name
    "" << ip << ":" << port.to_s << "-" << name
  end
  
  def add_device d
    Chef::Log.debug "added device @ip #{d.ip}:#{d.port}"
    key = RingInfo.dev_key d.ip,d.port ,d.name
    @devices[key] = d
  end
  
  def to_s
    s=""
    #s <<"r:" << @replicas <<"z:" << @zones
    devices.each { |d|
      s << "\n  " << d.to_s
    }
  end
end



def load_current_resource
  name = @new_resource.name
  name = "/etc/swift/#{name}"
  @current_resource = Chef::Resource::SwiftRingfile.new(name)
  @ring_test = nil  
  Chef::Log.info("parsing ring-file for #{name}")
  IO.popen("swift-ring-builder #{name}") { |pipe|
    ring_txt=pipe.readlines
    Chef::Log.debug("raw ring info:#{ring_txt}")
    @ring_test = scan_ring_desc ring_txt
    Chef::Log.debug("at end of load, current ring is: #{@ring_test.to_s}")
  }
  compute_deltas
end


def scan_ring_desc(input)
  
  r = RingInfo.new
  state = :init  
  next_state =''  # if the current state is ignore, this is the next state
  ignore_count = 0  # the number of lines to ignore
  input.each { |line| 
    
    case state
      when :init
      state=:gen_info
      next
      
      when :ignore
      Chef::Log.debug("ignoring line:" + line )
      ignore_count -= 1      
      if (ignore_count ==0)
        state = next_state
      end
      next
      
      when :gen_info 
      line =~/^(\d+).+,(\d+).+,(\d+).+,(\d+).*,([0-9.]+).+$/
      r.partitions=$1
      r.replicas=$2
      r.zones=$3
      r.device_num=$4
      state = :ignore      
      next_state = :dev_info
      ignore_count =2
      next
      
      when :dev_info  #              0     1 192.168.124.131  6002      sdb1 100.00          0 -100.00
      Chef::Log.debug "reading dev info:" + line
      line =~ /^\s+(\d+)\s+(\d+)\s+(\d+\.\d+\.\d+\.\d+)\s+(\d+)\s+(\S+)\s+([0-9.]+)\s+(\d+)\s+([-0-9.]+)\s*$/
      if $~.nil? 
        raise "failed to parse: #{line}"
      else
        Chef::Log.debug "matched: #{$~[0]}" 
      end
      dev = RingInfo::RingDeviceInfo.new
      dev.id = $1
      dev.zone = $2
      dev.ip = $3
      dev.port = $4
      dev.name = $5
      dev.weight = $6
      dev.partitions = $7
      r.add_device dev
    end
  }
  r
end


###
# compute disks to be added or removed (and update the dirty flag)
def compute_deltas 
  req_disks = @new_resource.disks
  keyed_req = {}  # for easy lookup, make a map of the requested disks 
  cur = @ring_test
  name = @new_resource.name
  @to_add = []
  @to_rem = []
  
  
  ## figure out which disks need adding
  req_disks.each {|disk| 
    key = RingInfo.dev_key disk[:ip],disk[:port],disk[:dev_name]
    @to_add << disk unless cur and cur.devices[key] # add unless present
    keyed_req[key] = disk
  } 
  
  ### figure out which disks need removing
  cur.devices.each {|key, d|
    @to_rem << d unless keyed_req[key] # remove unless still requested
  } if cur
    
  Chef::Log.info("disks, to add #{@to_add.length} , to remove: #{@to_rem.length}" ) 
  Chef::Log.debug("disks, to add #{@to_add.join(";")} , to remove: #{@to_rem.join(";")}" )
  
end

action :apply do
  name = @new_resource.name
  cur=@ring_test
  Chef::Log.info("current content of: #{name} #{(cur.nil? ? "-not there" : cur.to_s)}")
  
  ## make sure file exists
  create_ring
  
  # if we're changing the ring, make sure that file timestamps differ somewhat
  if @to_add.length > 0 or @to_rem.length > 0
    sleep 0.1
  end
  
  @to_add.each { |d|
    execute "add disk #{d[:ip]}:#{d[:port]}/#{d[:dev_name]} to #{name}" do
      command "swift-ring-builder #{name} add z#{d[:zone]}-#{d[:ip]}:#{d[:port]}/#{d[:dev_name]} #{d[:weight]}"
      cwd "/etc/swift"
      returns 1
    end
  }
  
  @to_rem.each {|d|
    execute "remove disk #{d.id} from #{name}" do
      command "swift-ring-builder #{name} remove d#{d.id} "
      cwd "/etc/swift"
      returns 1
    end   
  }  
end


action :rebalance do
  name = @current_resource.name   
  dirty = false
  
  ring_data_mtime= ::File.new(name).mtime   if ::File.exist?(name)
  ring_data_mtime ||= File.new(name).mtime   if ::File.exist?(name)
  ring_data_mtime ||= 0
  ring_name = name.sub(/^(.*)\..*$/, '\1.ring.gz')  
  ring_file_mtime = (::File.exist?(ring_name) ? ::File.mtime(ring_name) : -1)
  dirty = true if (ring_data_mtime.to_i > ring_file_mtime.to_i) 
  
  Chef::Log.info("current status for: #{name} is #{dirty ? "dirty" : "not-dirty"} #{ring_name} #{ring_data_mtime.to_i}/#{ring_file_mtime.to_i}")
 
  execute "rebalance ring for #{name}" do    
    command "swift-ring-builder #{name} rebalance"
    cwd "/etc/swift"
    returns [0,1]  # returns 1 if it didn't do anything, 2 on 
  end if dirty

  # if no rebalance was needed, but the the ring file is not there, make sure to make it.
  if !::File.exist?(ring_name) then 
    dirty = true
    execute "writeout ring for #{name}" do
      command "swift-ring-builder #{name} write_ring"
      cwd "/etc/swift"
      returns [0,1]  ## returns 1 if it didn't do anything, 2 on error.
    end
  end 

  @new_resource.updated_by_last_action(dirty)
end



def create_ring
  name = @new_resource.name    
  mh = @new_resource.min_part_hours ? @new_resource.min_part_hours : 1 
  parts = @new_resource.partitions ? @new_resource.partitions : 18
  replicas = @new_resource.replicas ? @new_resource.replicas : 3
  
  execute "create #{name} ring" do
    command "swift-ring-builder #{name} create #{parts}  #{replicas} #{mh}"
    creates "/etc/swift/#{name}"
    cwd "/etc/swift"
  end
end


if __FILE__ == $0
# produced by:
# root@d00-0c-29-14-30-92:/etc/swift# swift-ring-builder account.builder

test_str=<<TEST
account.builder, build version 1
262144 partitions, 1 replicas, 1 zones, 1 devices, 100.00 balance
The minimum number of hours before a partition can be reassigned is 1
Devices:    id  zone      ip address  port      name weight partitions balance meta
             0     1 192.168.124.131  6002      sdb1 100.00          0 -100.00
TEST

r = scan_ring_desc test_str.lines
puts "no r for you \n\n\n" if r.nil?
puts r.to_s

end