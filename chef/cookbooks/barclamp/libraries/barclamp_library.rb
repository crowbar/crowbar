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
        intf_to_if_map = Barclamp::Inventory.build_node_map
        node[:crowbar][:network].each do |net, data|
          intf, interface_list = Barclamp::Inventory.lookup_interface_info(node, data["conduit"], intf_to_if_map)
          answer << Network.new(net, data, intf, interface_list)
        end unless node[:crowbar][:network].nil?
        answer
      end

      def self.get_network_by_interface(node, intf)
        node[:crowbar][:network].each do |net, data|
          next if net != intf
          intf, interface_list = Barclamp::Inventory.lookup_interface_info(node, data["conduit"])
          return Network.new(net, data, intf, interface_list)
        end unless node[:crowbar][:network].nil?
        nil
      end

      def self.get_network_by_type(node, type)
        node[:crowbar][:network].each do |net, data|
          next if data[:usage] != type
          intf, interface_list = Barclamp::Inventory.lookup_interface_info(node, data["conduit"])
          return Network.new(net, data, intf, interface_list)
        end unless node[:crowbar][:network].nil?
        node[:crowbar][:network].each do |net, data|
          next if data[:usage] != "admin"
          intf, interface_list = Barclamp::Inventory.lookup_interface_info(node, data["conduit"])
          return Network.new(net, data, intf, interface_list)
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


      def self.bus_index(bus_order, path)
        Chef::Log.fatal("GREG: bus_index: b:#{bus_order} p:#{path}")
        return -1 if bus_order.nil?

        dpath = path.split("/.")

        index = 0
        bus_order.each do |b|
          subindex = 0
          b.split("/.")

          match = true
          b.each do |bp|
            match = false if subindex >= dpath.size or bp != dpath[subindex]
            break unless match
            subindex = subindex + 1
          end

          return index if match
          index = index + 1
        end

        -1
      end

      def self.sort_ifs(map, bus_order)
        Chef::Log.fatal("GREG: pre sort answer = #{map.inspect}")
        answer = map.sort{|a,b|
          Chef::Log.fatal("GREG: compare a:#{a.inspect} b:#{b.inspect}")
          aindex = Barclamp::Inventory.bus_index(bus_order, a[1])
          bindex = Barclamp::Inventory.bus_index(bus_order, b[1])
          aindex == bindex ? a[0] <=> b[0] : aindex <=> bindex
        }
        Chef::Log.fatal("GREG: post sort answer = #{answer.inspect}")
        answer.map! { |x| x[0] }
      end

      def self.get_bus_order(node)
        bus_order = nil
        node["network"]["interface_map"].each do |data|
          bus_order = data["bus_order"] if node[:dmi][:system][:product_name] =~ /#{data["pattern"]}/
          break if bus_order
        end
        Chef::Log.fatal("GREG: bus_order is: #{bus_order} for #{node[:dmi][:system][:product_name]}")
        bus_order
      end

      def self.get_conduits(node)
        conduits = nil
        node["network"]["conduit_map"].each do |data|
          parts = data["pattern"].split("/")
          the_one = true
          the_one = false unless node["network"]["mode"] =~ /#{parts[0]}/
          the_one = false unless node["crowbar"]["detected"]["network"].size.to_s =~ /#{parts[1]}/

          found = false
          node.roles.each do |role|
            found = true if role =~ /#{parts[2]}/
            break if found
          end
          the_one = false unless found

          Chef::Log.fatal("GREG: full data conduits: #{data.inspect}") if the_one
          conduits = data["conduit_list"] if the_one
          break if conduits
        end
        conduits
      end

      def self.build_node_map(node)
        bus_order = Barclamp::Inventory.get_bus_order(node)
        conduits = Barclamp::Inventory.get_conduits(node)

        return {} if conduits.nil?

        Chef::Log.fatal("GREG: using conduits: #{conduits.inspect}")

        if_list = node["crowbar"]["detected"]["network"]

        sorted_ifs = Barclamp::Inventory.sort_ifs(if_list, bus_order)
        Chef::Log.fatal("GREG: sorted_ifs = #{sorted_ifs.inspect}")
        if_remap = {}
        count = 1
        sorted_ifs.each do |intf|
          if_remap["1g#{count}"] = intf
          count = count + 1
        end

        Chef::Log.fatal("GREG: if_remap = #{if_remap.inspect}")
        ans = {}
        conduits.each do |k,v|
          Chef::Log.fatal("GREG: doctoring = #{v.inspect} for #{k}")
          hash = Hash.new(v)
          hash["if_list"] = v["if_list"].map do |y|
            if_remap[y]
          end
          ans[k] = hash
        end

        Chef::Log.fatal("GREG: return from build_node_map: #{ans.inspect}")
        ans
      end

      def self.lookup_interface_info(node, conduit, intf_to_if_map = nil)
        intf_to_if_map = Barclamp::Inventory.build_node_map(node) if intf_to_if_map.nil?

        return [nil, nil] if intf_to_if_map[conduit].nil?

        c_info = intf_to_if_map[conduit]
        interface_list = c_info["if_list"]

        return [interface_list[0], interface_list] if interface_list.size == 1
        id = gsub(conduit, "intf")
        ["bond#{id}", interface_list]
      end

      class Network
        attr_reader :name, :address, :broadcast, :mac, :netmask, :subnet, :router, :usage, :vlan, :use_vlan, :interface, :interface_list, :add_bridge, :conduit
        def initialize(net, data, rintf, interface_list)
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
          @conduit = data["conduit"]
          @interface = rintf
          @interface_list = interface_list
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


