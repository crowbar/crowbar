
action :add do
  filename = "/opt/dell/openstack_manager/htdigest"
  digest = Digest::MD5.hexdigest("#{new_resource.name}:#{new_resource.description}:#{new_resource.password}")
  utils_line "#{new_resource.name}:#{new_resource.description}:#{digest}" do
    action :add
    file filename
  end
end

action :remove do
  filename = "/opt/dell/openstack_manager/htdigest"
  digest = Digest::MD5.hexdigest("#{new_resource.name}:#{new_resource.description}:#{new_resource.password}")
  utils_line "#{new_resource.name}:#{new_resource.description}:#{digest}" do
    action :remove
    file filename
  end
end

