

action :add do
  utils_line "#{new_resource.ipaddress} #{new_resource.hostname}" do
    action :add
    file "/etc/bind/hosts"
    notifies :run, resources(:bash => "build-domain-file"), :delayed
  end
end

action :remove do
  utils_line "#{new_resource.ipaddress} #{new_resource.hostname}" do
    action :remove
    file "/etc/bind/hosts"
    notifies :run, resources(:bash => "build-domain-file"), :delayed
  end
end

