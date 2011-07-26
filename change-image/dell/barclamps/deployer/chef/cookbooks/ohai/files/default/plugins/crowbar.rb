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

require 'timeout'

provides "crowbar"

class System
  def self.background_time_command(timeout, background, name, command)
    File.open("/tmp/tcpdump-#{name}.sh", "w+") { |fd|
      fd.puts("#!/bin/bash")
      fd.puts("#{command} &")
      fd.puts("sleep #{timeout}")
      fd.puts("kill %1")
    }

    system("chmod +x /tmp/tcpdump-#{name}.sh")
    if background
      system("/tmp/tcpdump-#{name}.sh &")
    else
      system("/tmp/tcpdump-#{name}.sh")
    end
  end
end

filename = "/usr/sbin/lshw"
if !File.exists?(filename)
  filename = "/usr/bin/lshw"
end
f = IO.popen("#{filename} -class network | egrep 'logical|bus info|-network|serial'")
networks = []
mac_map = {}
bus_found=false
logical_name=""
mac_addr=""
wait=false
f.each { |line|
  if line =~ /\*-network/
    if bus_found
      networks << logical_name
      mac_map[logical_name] = mac_addr 
      if !File.exists?("/tmp/tcpdump.#{logical_name}.out") 
        System.background_time_command(45, true, logical_name, "ifconfig #{logical_name} up ; tcpdump -c 1 -lv -v -i #{logical_name} -a -e -s 1514 ether proto 0x88cc > /tmp/tcpdump.#{logical_name}.out")
        wait=true
      end
    end
    bus_found=false
  end
  if line =~ /bus info:/
    bus_found = true
  end
  if line =~ /logical name: (.*)/
    logical_name = $1
  end
  if line =~ /serial: (.*)/
    mac_addr = $1
  end
}
f.close
if bus_found
  networks << logical_name
  mac_map[logical_name] = mac_addr 
  if !File.exists?("/tmp/tcpdump.#{logical_name}.out") 
    System.background_time_command(45, true, logical_name, "ifconfig #{logical_name} up ; tcpdump -c 1 -lv -v -i #{logical_name} -a -e -s 1514 ether proto 0x88cc > /tmp/tcpdump.#{logical_name}.out")
    wait=true
  end
end
system("sleep 45") if wait

crowbar Mash.new
crowbar[:switch_config] = Mash.new unless crowbar[:switch_config]

networks.each do |network|
  sw_port = -1
  line = %x[cat /tmp/tcpdump.#{network}.out | grep "Subtype Interface Name"]
  if line =~ /[\d]+\/[\d]+\/([\d]+)/
    sw_port = $1
  end
  if line =~ /: Unit [\d]+ Port ([\d]+)/
    sw_port = $1
  end

  sw_unit = -1
  line = %x[cat /tmp/tcpdump.#{network}.out | grep "Subtype Interface Name"]
  if line =~ /([\d]+)\/[\d]+\/[\d]+/
    sw_unit = $1
  end
  if line =~ /: Unit ([\d]+) Port [\d]+/
    sw_unit = $1
  end

  sw_name = -1
  # GREG: Using mac for now, but should change to something else later.
  line = %x[cat /tmp/tcpdump.#{network}.out | grep "Subtype MAC address"]
  if line =~ /: (.*) \(oui/
    sw_name = $1
  end

  crowbar[:switch_config][network] = Mash.new unless crowbar[:switch_config][network]
  crowbar[:switch_config][network][:interface] = network
  crowbar[:switch_config][network][:mac] = mac_map[network].downcase
  crowbar[:switch_config][network][:switch_name] = sw_name
  crowbar[:switch_config][network][:switch_port] = sw_port
  crowbar[:switch_config][network][:switch_unit] = sw_unit
end

