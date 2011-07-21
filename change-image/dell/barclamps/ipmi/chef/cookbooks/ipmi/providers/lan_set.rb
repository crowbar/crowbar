
## utility to check if a lan parameter needs to be set 
## (if it's current value is different than desired one).
def check_ipmi_lan_value(header, desired)
  c_awk = "awk -F: '/^#{header}[ \\t]*:/ { print $2 }'"
  current = %x{ipmitool lan print 1 | #{c_awk} }
  current = current.chomp.strip
  current.casecmp(desired) == 0
end

action :run do
  name = new_resource.name
  command = new_resource.command
  value = new_resource.value
  settle_time = new_resource.settle_time

  if ::File.exists?("/sys/module/ipmi_si")
    unless check_ipmi_lan_value(name, value)
      # Set BMC LAN parameters 
      bash "#{name} settle time" do
        code "sleep #{settle_time}"
        action :nothing
      end

      bash "bmc-set-lan-#{name}" do
        code <<-EOH
#{command}
EOH
        notifies :run, resources(:bash => "#{name} settle time"), :immediately
      end 

      node["crowbar"]["status"]["ipmi"]["messages"] << "#{name} set to #{value}" unless node.nil?
    else
      node["crowbar"]["status"]["ipmi"]["messages"] << "#{name} already set to #{value}" unless node.nil?
    end

    node["crowbar"]["status"]["ipmi"]["address_set"] = true if name == "IP Address"
  else
    node["ipmi"]["bmc_enable"] = false
    node["crowbar"]["status"]["ipmi"]["messages"] << "Unsupported product found #{node[:dmi][:system][:product_name]} - skipping IPMI:#{name}" unless node.nil?
  end  
  node.save
end

