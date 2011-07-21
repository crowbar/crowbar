

action :add do
  utils_line "-n #{new_resource.subnet} -N #{new_resource.netmask}" do
    action :add
    file "/etc/bind/netargs"
    notifies :run, resources(:bash => "build-domain-file"), :delayed
  end
end

action :remove do
  utils_line "-n #{new_resource.subnet} -N #{new_resource.netmask}" do
    action :remove
    file "/etc/bind/netargs"
    notifies :run, resources(:bash => "build-domain-file"), :delayed
  end
end

