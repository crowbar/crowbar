# GANLIA monitoring server recipe

include_recipe "ganglia::client"

package "gmetad"
package "ganglia-webfrontend"

# Begin recipe transactions
Chef::Log.debug("BEGIN ganglia-server")

link "/etc/apache2/conf.d/ganglia.conf" do
  to "/etc/ganglia-webfrontend/apache.conf"
  not_if "test -L /etc/apache2/conf.d/ganglia.conf"
  notifies :reload, "service[apache2]"
end

template "/etc/ganglia/gmetad.conf" do
  source "gmetad.conf.erb" 
  notifies :restart, "service[gmetad]"
end

service "gmetad" do
  supports :restart => true
  running true
  enabled true
  action [ :enable, :start ]
end

# End of recipe transactions
Chef::Log.debug("END ganglia-server")

