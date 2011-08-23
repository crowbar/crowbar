default[:erlang][:version] = "R14B02"
default[:erlang][:source]  = "http://erlang.org/download/otp_src_#{erlang[:version]}.tar.gz"
default[:erlang][:path]    = "/var/vcap/deploy/rubies/erlang"

default[:deployment][:group] = "1000"
default[:deployment][:name] = "devbox"
default[:deployment][:config_path] = "/home/openstack/.cloudfoundry/devbox/config"
default[:deployment][:user] = "openstack"
default[:deployment][:home] = "/home/openstack/.cloudfoundry/devbox"
