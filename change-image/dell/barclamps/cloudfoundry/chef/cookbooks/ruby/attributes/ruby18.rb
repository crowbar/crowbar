default[:ruby18][:version] = "1.8.7-p334"
default[:ruby18][:source]  = "http://ftp.ruby-lang.org//pub/ruby/1.8/ruby-#{ruby18[:version]}.tar.gz"
default[:ruby18][:path]    = "/var/vcap/deploy/rubies/ruby-#{ruby18[:version]}"
default[:rubygems][:version] = "1.7.2"
default[:rubygems][:bundler][:version] = "1.0.12"
default[:rubygems][:rake][:version] = "0.8.7"

default[:deployment][:group] = "1000"
default[:deployment][:name] = "devbox"
default[:deployment][:config_path] = "/home/openstack/.cloudfoundry/devbox/config"
default[:deployment][:user] = "openstack"
default[:deployment][:home] = "/home/openstack/.cloudfoundry/devbox"

