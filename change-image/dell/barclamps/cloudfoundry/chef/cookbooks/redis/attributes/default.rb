default[:redis][:version] = "2.2.1"
default[:redis][:path] = "/var/lib/redis-#{redis[:version]}"
default[:redis][:runner] = "redis"
default[:redis][:port] = 6379
default[:redis][:password] = "redis"

default[:deployment][:group] = "1000"
default[:deployment][:name] = "devbox"
default[:deployment][:config_path] = "/home/openstack/.cloudfoundry/devbox/config"
default[:deployment][:user] = "openstack"
default[:deployment][:home] = "/home/openstack/.cloudfoundry/devbox"