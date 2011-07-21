define :glance_service do

  glance_name="glance-#{params[:name]}"

  service glance_name do
    if (platform?("ubuntu") && node.platform_version.to_f >= 10.04)
      restart_command "restart #{glance_name}"
      stop_command "stop #{glance_name}"
      start_command "start #{glance_name}"
      status_command "status #{glance_name} | cut -d' ' -f2 | cut -d'/' -f1 | grep start"
    end
    supports :status => true, :restart => true
    action [:enable, :start]
    subscribes :restart, resources(:template => node[:glance][:config_file])
  end

end
