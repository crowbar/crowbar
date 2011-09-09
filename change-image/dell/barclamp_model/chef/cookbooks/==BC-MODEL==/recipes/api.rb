#
# Cookbook Name:: glance
# Recipe:: api
#
#

include_recipe "#{@cookbook_name}::common"

==BC-MODEL==_service "api"

node[:==BC-MODEL==][:monitor][:svcs] <<["==BC-MODEL==-api"]

