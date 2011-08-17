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

require 'chef/mixin/shell_out'
include Chef::Mixin::ShellOut


def load_current_resource
  dev_name = @new_resource.name
  @current = Chef::Resource::SwiftDisk.new(dev_name)
  
  parted_partition_parse  dev_name
  geo = @current.geo()  
  if geo.nil? or geo[:cyl_size].nil? 
    ### if the device has no good partition table, parted returns nothing useful
    ### use sfdisk to get the info
    Chef::Log.info("parted didn't geo, running sfdisk")
    sfdisk_get_geo dev_name
    geo = @current.geo()
  end
  
  parts = @current.part()
  
  s = <<EOF
current state for dev #{dev_name}
  Geometry cyls:#{geo[:cyl]} heads: #{geo[:head]} sector: #{geo[:sector]}      
EOF
  
  num = 1
  parts.each { | p | 
    s << "partition " << num  
    s << " start/end/cyl-count: " << p[:start] + "/"+ p[:end] + "/" + p[:size].to_s 
    s << " type:" << p[:type]
    s << "\n"
    num+=1
  } if !parts.nil?
  Chef::Log.info(s)
end


def set_geo(cyl,head,sector, cyl_size =0)
  Chef::Log.info("updating geo to: #{cyl}/#{head}/#{sector}/#{cyl_size}")
  @current.geo({
              :cyl => cyl.to_f,
              :head => head.to_f,
              :sector => sector.to_f,
              :cyl_size => cyl_size.to_f
  })
  @current.geo[:cyl_size_computed] = @current.geo[:head].to_f * @current.geo[:sector].to_f * 512
end

=begin
sample output
# sfdisk /dev/sdb -g
/dev/sdb: 261 cylinders, 255 heads, 63 sectors/track
=end
def sfdisk_get_geo(dev_name)
  out = %x{sfdisk #{dev_name} -g}
  Chef::Log.info("updating geo using sfdisk: #{out}")
  
  out =~ /^(.*):[ ]*([0-9]+).*,[ ]*([0-9]+).*,[ ]*([0-9]+).*$/
  set_geo($2,$3,$4)
end


def parted_partition_parse(dev_name)
  Chef::Log.debug("reading partition table for #{dev_name}")
=begin  
Run parted to get basic info about the disk
sample output:
~# parted -m -s /dev/sda unit cyl print 
CYL;
/dev/sda:1044cyl:scsi:512:512:msdos:VMware Virtual disk;
1044:255:63:8225kB;
1:0cyl:31cyl:30cyl:ext2::boot;
2:31cyl:1044cyl:1012cyl:::;
5:31cyl:1044cyl:1012cyl:::lvm;
=end
  pipe= IO.popen("parted -m -s #{dev_name} unit cyl print") # this can return 1, but it's ok (if no partition table present, we'll create it)
  result = pipe.readlines
  parted_parse_results result  
end

def parted_parse_results(input)
  Chef::Log.debug("read:" + input.inspect)
  input = input.to_a
  part_tab = []  
  catch (:parse_error) do
    line = input.shift
    throw :parse_error if line =~ /^Error:/
    line = input.shift # CYL;  
    throw :parse_error unless line =~ /\/dev\/([^\/]+):([0-9]+)cyl:(.*):.*$/          
    
    line = input.shift # 1044:255:63:8225kB;  
    throw :parse_error unless line =~ /([0-9]+):([0-9]+):([0-9]+):(([0-9]+)([a-zA-Z]*));$/
    set_geo(Regexp.last_match(1), Regexp.last_match(2), Regexp.last_match(3), size_to_bytes(Regexp.last_match(4)))
    
    input.each { |line|
      # 1:0cyl:31cyl:30cyl:ext2::boot;
      throw :parse_error unless line =~ /([0-9]):([0-9]+)cyl:([0-9]+)cyl:([0-9]+)cyl:(.*):(.*);$/
      part_num = Regexp.last_match(1).to_i
      part_info = {
            :num => part_num,
            :start => Regexp.last_match(2),
            :end => Regexp.last_match(3),
            :size => size_to_bytes(Regexp.last_match(4)),
            :type => Regexp.last_match(5),
            :system => Regexp.last_match(6),
            :flags => Regexp.last_match(7) }
      part_tab << part_info
    }
  end
  
  @current.part(part_tab)
  part_tab  
end


def size_to_bytes(s)  
  case s
    when /^([0-9]+)$/
    return $1.to_f
    
    when /^([0-9]+)[Kk][Bb]$/
    return $1.to_f * 1024
    
    when /^([0-9]+)[Mm][Bb]$/
    return $1.to_f * 1024 * 1024
    
    when /^([0-9]+)[Gg][Bb]$/
    return $1.to_f * 1024 * 1024 * 1024
    
    when /^([0-9]+)[Tt][Bb]$/
    return $1.to_f * 1024 * 1024 * 1024 * 1024
    
  end
  -1
end

action :list do 
  Chef::Log.info("at some point there'll be a list")
end

#### 
# compare the requested partition table parameters to what exists
# if differences found - remove all current partitions, and create new ones.
# An existing partition is considered a match if:
#  - it has the same serial # (1,2,3)
#  - it has the same size

action :ensure_exists do
  req = @new_resource.part
  cur = @current.part
  dev_name = @new_resource.name
  
  recreate, delete_existing  = false
  cyl_size = @current.geo[:cyl_size_computed]     
  
  if (cur.nil?)
    recreate = true;
  else
    cur_cyl = 0
    idx = 0
    req.each { |params|
      if (cur[idx].nil?)
        recreate = true
        Chef::Log.info("no current #{idx}")
        next
      end 
      req_size = params[:size]
      if (req_size == :remaining)
        req_size = (@current.geo[:cyl] - cur_cyl -1) * cyl_size 
      end
      cur_size = cur[idx][:size] * cyl_size
      cur_min, cur_max = req_size*0.9, req_size*1.1
      if !(cur_size > cur_min and cur_size < cur_max)
        recreate = true
      end
      cur_cyl += cur[idx][:size]
      idx+=1
      Chef::Log.info("partition #{idx} #{(recreate ? 'differs' : 'is same')}: #{cur_size}/ #{req_size}")
    } 
  end 
  
  if !recreate
    Chef::Log.info("partition table matches - not recreating")
    @current.updated_by_last_action(false)
  else
    @current.updated_by_last_action(true)
    
    ### make sure to ensure that there are no mounted 
    ### filesystems on the device
    re = /^(#{Regexp.escape(dev_name)}[0-9]+)/
    mounted = []  
    shell_out!("mount").stdout.each_line { |line|
      md = re.match(line)
      next unless md      
      mounted << md[1]
    }
    mounted.each { |m|
      Chef::Log.info("unmounting #{m}")
      shell_out!("umount #{m}")
    }
    
    # Nuke current partition table.
    execute "create new partition table" do
      command "parted -s -m #{dev_name} mktable bsd"
    end

    # create new partitions    
    cur_cyl = 0
    idx = 0
    req.each { | params |  
      if (params[:size] == :remaining)
        cyls = (@current.geo[:cyl] - cur_cyl - 1)  
      else 
        cyls = (params[:size].to_f / cyl_size.to_f).floor.to_i
      end
      s = "parted -m -s #{dev_name} unit cyl "
      s << "mkpart #{cur_cyl.to_i} #{(cur_cyl += cyls).to_i} " # #{params[:type]}    
      Chef::Log.info("creating new partition #{idx+1} with:" + s)
      execute "creating partition #{idx}" do 
        command s
      end        
      idx+=1
    }
  end
end

