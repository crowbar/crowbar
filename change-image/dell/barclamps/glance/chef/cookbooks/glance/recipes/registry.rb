#
# Cookbook Name:: glance
# Recipe:: registry
#
#

include_recipe "#{@cookbook_name}::common"

glance_service "registry"

node[:glance][:monitor][:svcs] << ["glance-registry"]
