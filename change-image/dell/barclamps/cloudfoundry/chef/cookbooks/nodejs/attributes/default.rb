default[:nodejs][:version] = "0.4.7"
default[:nodejs][:path] = "/var/vcap/deploy/nodejs"
default[:nodejs][:source] = "http://nodejs.org/dist/node-v#{nodejs[:version]}.tar.gz"

default[:deployment][:group] = "1000"
default[:deployment][:name] = "devbox"
default[:deployment][:config_path] = "/home/openstack/.cloudfoundry/devbox/config"
default[:deployment][:user] = "openstack"
default[:deployment][:home] = "/home/openstack/.cloudfoundry/devbox"