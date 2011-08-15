#
# Cookbook Name:: glance
# Recipe:: api
#
#

include_recipe "#{@cookbook_name}::common"

cloudfoundry_service "api"

node[:cloudfoundry][:monitor][:svcs] <<["cloudfoundry-api"]

