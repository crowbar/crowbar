#
# Cookbook Name:: glance
# Recipe:: api
#
#

include_recipe "#{@cookbook_name}::common"

glance_service "api"

node[:glance][:monitor][:svcs] <<["glance-api"]

