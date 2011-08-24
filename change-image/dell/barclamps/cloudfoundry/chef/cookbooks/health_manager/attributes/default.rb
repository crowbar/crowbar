include_attribute "postgresql"
default[:health_manager][:config_file] = "health_manager.yml"
default[:health_manager][:database][:username] = node[:postgresql][:server_root_user]
default[:health_manager][:database][:password] = node[:postgresql][:server_root_password]

default[:deployment][:group] = "1000"
default[:deployment][:name] = "devbox"
default[:deployment][:config_path] = "/home/openstack/.cloudfoundry/devbox/config"
default[:deployment][:user] = "openstack"
default[:deployment][:home] = "/home/openstack/.cloudfoundry/devbox"
