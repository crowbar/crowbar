
dvd = "#{node[:platform]}_dvd"

domain_name = node[:dns].nil? ? node[:domain] : (node[:dns][:domain] || node[:domain])
admin_ip = Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, "admin").address
admin_net = node[:network][:networks]["admin"]
dhcp_start = admin_net[:ranges]["dhcp"]["start"]
dhcp_end = admin_net[:ranges]["dhcp"]["end"]
lease_time = node[:provisioner][:dhcp]["lease-time"]

dhcp_groups = { "hwinstall" => 1, "update" => 3, "execute" => 4}
dhcp_groups.each do |group, dhcp_state|
  dhcp_group group do
    action :add
    options [ "option domain-name \"#{domain_name}\"",
              "option dhcp-client-state #{dhcp_state}",
              "filename \"/#{dvd}/#{group}/pxelinux.0\"" ]
  end
end


dhcp_subnet admin_net["subnet"] do
  action :add
  broadcast admin_net["broadcast"]
  netmask admin_net["netmask"]
  routers (admin_net["router"].nil? ? [] : [ admin_net["router"] ])
  options [ "option domain-name \"#{domain_name}\"",
            "option domain-name-servers #{admin_ip}",
            "range #{dhcp_start} #{dhcp_end}",
            "default-lease-time #{lease_time}",
            "max-lease-time #{lease_time}",
            "filename \"/#{dvd}/discovery/pxelinux.0\"" ]
end
