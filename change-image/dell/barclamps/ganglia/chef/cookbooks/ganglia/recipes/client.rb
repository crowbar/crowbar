# GANLIA monitoring client recipe

package "ganglia-monitor"

# Begin recipe transactions
Chef::Log.debug("BEGIN ganlia-client")

admin_interface = Ganglia::Evaluator.get_value_by_type(node,:interface_eval)

template "/etc/ganglia/gmond.conf" do
  source "gmond.conf.erb" 
  variables( :admin_interface => admin_interface )
  notifies :restart, "service[ganglia-monitor]"
end

service "ganglia-monitor" do
  supports :restart => true
  pattern "gmond"
  running true
  enabled true
  action [ :enable, :start ]
end

# End of recipe transactions
Chef::Log.debug("END ganglia-client")

