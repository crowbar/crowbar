default[:nats][:host] = "localhost"
default[:nats][:port] = "4222"
default[:nats][:user] = "nats"
default[:nats][:password] = "nats"

default[:deployment][:group] = "1000"
default[:deployment][:name] = "devbox"
default[:deployment][:config_path] = "/home/openstack/.cloudfoundry/devbox/config"
default[:deployment][:user] = "openstack"
default[:deployment][:home] = "/home/openstack/.cloudfoundry/devbox"

default[:ruby][:version] = "1.9.2-p180"
default[:ruby][:path] = "/home/openstack/.cloudfoundry/devbox/deploy/rubies/ruby-1.9.2-p180"
