
action :add do
  filename = "/etc/dhcp3/groups.d/#{new_resource.name}.conf"
  template filename do 
    cookbook "dhcp"
    source "group.conf.erb"
    variables(
      :name => new_resource.name,
      :options => new_resource.options
    )
    owner "root"
    group "root"
    mode 0644
    notifies :restart, resources(:service => "dhcp3-server"), :delayed
  end
  utils_line "include \"#{filename}\";" do
    action :add
    file "/etc/dhcp3/groups.d/group_list.conf"
    notifies :restart, resources(:service => "dhcp3-server"), :delayed
  end
end

action :remove do
  filename = "/etc/dhcp3/groups.d/#{new_resource.name}.conf"
  if ::File.exists?(filename)
    Chef::Log.info "Removing #{new_resource.name} group from /etc/dhcp3/groups.d/"
    file filename do
      action :delete
      notifies :restart, resources(:service => "dhcp3-server"), :delayed
    end
    new_resource.updated_by_last_action(true)
  end
  utils_line "include \"#{filename}\";" do
    action :remove
    file "/etc/dhcp3/groups.d/group_list.conf"
    notifies :restart, resources(:service => "dhcp3-server"), :delayed
  end
end

