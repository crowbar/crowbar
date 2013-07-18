#!/usr/bin/ruby

require 'rubygems'
require 'ipaddr'
require 'json'

REQUIRED_NETWORKS = {
  'admin' => ['admin', 'dhcp', 'host', 'switch'],
  'bmc' => ['host'],
  'bmc_vlan' => ['host'],
  'nova_fixed' => ['dhcp', 'router'],
  'nova_floating' => ['host'],
  'os_sdn' => ['host'],
  'public' => ['dhcp', 'host'],
  'storage' => ['host']
}

if ARGV.length != 1
  puts "Invalid call to script."
  exit 1
end

ipv4_arg = ARGV[0]
ipv4 = IPAddr.new(ipv4_arg)

databag = JSON.load($stdin)


### Early validation

if not databag.has_key?('attributes')
  puts "Invalid network JSON: missing attribute: attributes"
  exit 1
end

if not databag['attributes'].has_key?('network')
  puts "Invalid network JSON: missing attribute: attributes.network"
  exit 1
end

if not databag['attributes']['network'].has_key?('teaming')
  puts "Invalid network JSON: missing attribute: attributes.network.teaming"
  exit 1
end

if not databag['attributes']['network'].has_key?('conduit_map')
  puts "Invalid network JSON: missing attribute: attributes.network.conduit_map"
  exit 1
end

if not databag['attributes']['network'].has_key?('networks')
  puts "Invalid network JSON: missing attribute: attributes.network.networks"
  exit 1
end


### Validation of simple attributes

mode = databag['attributes']['network']['mode']
if not ['single', 'dual', 'team'].include?(mode)
  puts "Invalid mode '#{mode}': must be one of 'single', 'dual', 'team'"
  exit 1
end

teaming_mode = databag['attributes']['network']['teaming']['mode']
if not Range.new(0, 6).include?(teaming_mode)
  puts "Invalid teaming mode '#{teaming_mode}': must be a value between 0 and 6, see https://www.kernel.org/doc/Documentation/networking/bonding.txt"
  exit 1
end


### Validation of networks

class CrowbarNetworkRange
  attr_reader :name
  attr_reader :start
  attr_reader :end
  attr_reader :start_addr
  attr_reader :end_addr

  def initialize name, json
    @name = name

    if not json.is_a?(Hash)
      raise "range '#{@name}' is not a hash"
    end

    @start = json['start']
    @end = json['end']

    @start_addr = IPAddr.new(@start)
    @end_addr = IPAddr.new(@end)
  end

  def validate
    if @start_addr > @end_addr
      raise "start of range '#{@name}' ('#{@range_start}') is greater than its end ('#{@range_end}')"
    end
  end
end

class CrowbarNetwork
  attr_reader :name
  attr_reader :subnet_addr
  attr_reader :broadcast_addr
  attr_reader :subnet_addr_full
  attr_reader :ranges

  def initialize name, json
    @name = name

    if not json.is_a?(Hash)
      raise "definition is not a hash"
    end

    if !json.has_key?('ranges') || !json['ranges'].is_a?(Hash) || json['ranges'].length == 0
      raise "no valid range defined"
    end

    @subnet = json['subnet']
    @netmask = json['netmask']
    @broadcast = json['broadcast']
    @router = json['router']

    @subnet_addr = IPAddr.new(@subnet)
    @netmask_addr = IPAddr.new(@netmask)
    @broadcast_addr = IPAddr.new(@broadcast)
    @router_addr = nil
    @router_addr = IPAddr.new(@router) unless @router.nil?

    @subnet_addr_full = IPAddr.new("#{@subnet}/#{@netmask}")

    @ranges = {}

    json['ranges'].each do |range_name, range_value|
      @ranges[range_name] = CrowbarNetworkRange.new(range_name, range_value)
    end
  end

  def validate
    netmask_bits = @netmask_addr.to_i.to_s(2).count("1")

    sub = IPAddr.new("#{@subnet}/#{@netmask_bits}")
    if sub != @subnet_addr_full
      raise "invalid netmask '#{@netmask}'"
    end

    if @subnet_addr != @subnet_addr&@netmask_addr
      raise "invalid subnet '#{@subnet}' for netmask '#{@netmask}'"
    end

    if @broadcast_addr != @subnet_addr|~@netmask_addr
      raise "invalid broadcast '#{@broadcast}' for subnet '#{@subnet}/#{@netmask}' (should be '#{(@subnet_addr|~@netmask_addr).to_s}')"
    end

    unless @router.nil?
      if not @subnet_addr_full.include?(@router_addr)
        raise "invalid router '#{@router}' for subnet '#{@subnet}/#{@netmask}'"
      end
    end

    @ranges.each_value do |range|
      range.validate

      if not @subnet_addr_full.include?(range.start_addr)
        return "start of range '#{range.name}' ('#{range.start}') is not part of subnet '#{@subnet}/#{@netmask}'"
      end
      if not @subnet_addr_full.include?(range.end_addr)
        return "end of range '#{range.name}' ('#{range.end}') is not part of subnet '#{@subnet}/#{@netmask}'"
      end
    end

    ranges_array = @ranges.values.sort { |a,b| [a.start_addr, a.end_addr, a.name] <=> [b.start_addr, b.end_addr, b.name] }
    for i in 0..ranges_array.length-2 do
      if ranges_array[i].end_addr >= ranges_array[i+1].start_addr
        raise "ranges '#{ranges_array[i].name}' and '#{ranges_array[i+1].name}' are conflicting"
      end
    end
  end
end

def no_conflicting_ranges(neta, netb)
  error = false
  neta.ranges.each do |namea, rangea|
    netb.ranges.each do |nameb, rangeb|
      if neta.subnet_addr >= rangeb.start_addr && neta.subnet_addr <= rangeb.end_addr
        error = true
      elsif neta.broadcast_addr >= rangeb.start_addr && neta.broadcast_addr <= rangeb.end_addr
        error = true
      end
      raise "Network '#{neta.name}' and range '#{nameb}' from network '#{netb.name}' are conflicting" if error

      if rangea.start_addr >= rangeb.start_addr && rangea.start_addr <= rangeb.end_addr
        error = true
      elsif rangea.end_addr >= rangeb.start_addr && rangea.end_addr <= rangeb.end_addr
        error = true
      end
      raise "Range '#{namea}' from network '#{neta.name}' and range '#{nameb}' from network '#{netb.name}' are conflicting" if error
    end
  end
end


networks = databag['attributes']['network']['networks']

REQUIRED_NETWORKS.each do |name, ranges|
  if not networks.has_key?(name)
    puts "Missing definition of network '#{name}'"
    exit 1
  end

  ranges.each do |range|
    if not networks[name]['ranges'].has_key?(range)
      puts "Missing range '#{range}' in definition of network '#{name}'"
      exit 1
    end
  end
end

networks_hash = {}

networks.each do |name, value|
  begin
    net = CrowbarNetwork.new(name, value)
    net.validate
    networks_hash[name] = net
  rescue Exception => e
    puts "Cannot validate definition for network '#{name}': #{e.to_s}"
    exit 1
  end
end

if not networks_hash['public'].subnet_addr_full.include?(networks_hash['nova_floating'].subnet_addr_full)
  puts "'nova_floating' network must be a subnetwork of 'public' network"
  exit 1
end

if not networks_hash['admin'].subnet_addr_full.include?(networks_hash['bmc'].subnet_addr_full)
  puts "'bmc' network must be a subnetwork of 'admin' network"
  exit 1
end

if not networks_hash['admin'].subnet_addr_full.include?(networks_hash['bmc_vlan'].subnet_addr_full)
  puts "'bmc_vlan' network must be a subnetwork of 'admin' network"
  exit 1
end

begin
  networks_hash.each do |namea, neta|
    networks_hash.each do |nameb, netb|
      unless namea == nameb
        no_conflicting_ranges(neta, netb)
      end
    end
  end
rescue Exception => e
  puts "#{e.to_s}"
  exit 1
end


### Validation of IP address of admin node

admin_range = networks_hash['admin'].ranges['admin']

if ipv4 < admin_range.start_addr || ipv4 > admin_range.end_addr
  puts "admin range is configured from #{admin_range.start} to #{admin_range.end}"
  exit 1
end
