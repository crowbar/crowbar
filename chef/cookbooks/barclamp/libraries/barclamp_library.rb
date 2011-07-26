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

module BarclampLibrary
  class Barclamp
    class Inventory
      def self.list_networks(node)
        answer = []
        node[:crowbar][:network].each do |net, data|
          answer << Network.new(net, data)
        end unless node[:crowbar][:network].nil?
        answer
      end

      def self.get_network_by_interface(node, intf)
        node[:crowbar][:network].each do |net, data|
          next if net != intf
          return Network.new(net, data)
        end unless node[:crowbar][:network].nil?
        nil
      end

      def self.get_network_by_type(node, type)
        node[:crowbar][:network].each do |net, data|
          next if data[:usage] != type
          return Network.new(net, data)
        end unless node[:crowbar][:network].nil?
        node[:crowbar][:network].each do |net, data|
          next if data[:usage] != "admin"
          return Network.new(net, data)
        end unless node[:crowbar][:network].nil?
        Network.new(type, { "address" => node[:ipaddress] })
      end

      def self.list_disks(node)
        answer = []
        node[:crowbar][:disks].each do |disk, data|
          answer << Disk.new(disk, data)
        end unless node[:crowbar][:disks].nil?
        answer
      end

      class Network
        attr_reader :name, :address, :broadcast, :mac, :netmask, :subnet, :router, :usage, :vlan, :use_vlan, :interface, :interface_list, :add_bridge
        def initialize(net, data)
          @name = net
          @address = data["address"]
          @broadcast = data["broadcast"]
          @mac = data["mac"]
          @netmask = data["netmask"]
          @subnet = data["subnet"]
          @router = data["router"]
          @usage = data["usage"]
          @vlan = data["vlan"]
          @use_vlan = data["use_vlan"]
          @interface = data["interface"]
          @interface_list = data["interface_list"]
          @add_bridge = data["add_bridge"]
        end
      end

      class Disk
        attr_reader :name, :model, :removable, :rev, :size, :state, :timeout, :vendor, :usage
        def initialize(disk, data)
          @name = "/dev/#{disk}"
          @model = data["model"] || "Unknown"
          @removable = data["removable"] != "0"
          @rev = data["rev"] || "Unknown"
          @size = (data["size"] || 0).to_i
          @state = data["state"] || "Unknown"
          @timeout = (data["timeout"] || 0).to_i
          @vendor = data["vendor"] || "NA"
          @usage = data["usage"] || "Unknown"
        end

        def self.size_to_bytes(s)
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

      end

    end
  end
end


