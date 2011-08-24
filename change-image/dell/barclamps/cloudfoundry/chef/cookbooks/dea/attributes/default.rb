default[:dea][:config_file] = "dea.yml"

default[:deployment][:group] = "1000"
default[:deployment][:name] = "devbox"
default[:deployment][:config_path] = "/home/openstack/.cloudfoundry/devbox/config"
default[:deployment][:user] = "openstack"
default[:deployment][:home] = "/home/openstack/.cloudfoundry/devbox"

default[:cloudfoundry][:repo] = "https://github.com/cloudfoundry/vcap.git"
default[:cloudfoundry][:path] = "/var/vcap/src/vcap"