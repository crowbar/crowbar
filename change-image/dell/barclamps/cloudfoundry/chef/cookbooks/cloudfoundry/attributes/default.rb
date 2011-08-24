default[:cloudfoundry][:repo] = "https://github.com/cloudfoundry/vcap.git"
default[:cloudfoundry][:path] = "/var/vcap/src/vcap"

default[:deployment][:group] = "1000"
default[:deployment][:name] = "devbox"
default[:deployment][:config_path] = "/home/openstack/.cloudfoundry/devbox/config"
default[:deployment][:user] = "openstack"
default[:deployment][:home] = "/home/openstack/.cloudfoundry/devbox"

default[:ruby][:version] = "1.9.2-p180"
default[:ruby][:path] = "/home/openstack/.cloudfoundry/devbox/deploy/rubies/ruby-1.9.2-p180"