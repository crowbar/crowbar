default[:nginx][:worker_connections] = 2048
default[:nginx][:dir] = "/etc/nginx"
default[:nginx][:vcap_log] = "/var/log/nginx/vcap.access.log"

default[:deployment][:group] = "1000"
default[:deployment][:name] = "devbox"
default[:deployment][:config_path] = "/home/openstack/.cloudfoundry/devbox/config"
default[:deployment][:user] = "openstack"
default[:deployment][:home] = "/home/openstack/.cloudfoundry/devbox"