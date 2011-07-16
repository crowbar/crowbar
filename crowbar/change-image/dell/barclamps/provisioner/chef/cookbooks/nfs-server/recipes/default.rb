
package "nfs-common"
package "portmap"
package "nfs-kernel-server"

service "portmap" do
  running true
  enabled true
  action [ :enable, :start ]
end

link "/updates" do
  to "/tftpboot/ubuntu_dvd/updates"
end

directory "/install-logs" do
  owner "root"
  group "root"
  mode 0755
end

service "nfs-kernel-server" do
  supports :restart => true, :status => true, :reload => true
  running true
  enabled true
  action [ :enable, :start ]
end

template "/etc/exports" do
  source "exports.erb"
  group "root"
  owner "root"
  mode 0644
  variables(:admin_subnet => node["network"]["networks"]["admin"]["subnet"],
            :admin_netmask => node["network"]["networks"]["admin"]["netmask"])
  notifies :run, "execute[nfs-export]", :delayed
end

execute "nfs-export" do
  command "exportfs -a"
  action :run
end

