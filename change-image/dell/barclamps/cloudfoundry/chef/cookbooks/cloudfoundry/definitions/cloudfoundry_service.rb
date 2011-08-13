define :cloudfoundry_service do

  cloudfoundry_name="cloudfoundry-#{params[:name]}"

  service cloudfoundry_name do
    if (platform?("ubuntu") && node.platform_version.to_f >= 10.04)
      restart_command "restart #{cloudfoundry_name}"
      stop_command "stop #{cloudfoundry_name}"
      start_command "start #{cloudfoundry_name}"
      status_command "status #{cloudfoundry_name} | cut -d' ' -f2 | cut -d'/' -f1 | grep start"
    end
    supports :status => true, :restart => true
    action [:enable, :start]
    subscribes :restart, resources(:template => node[:cloudfoundry][:config_file])
  end

end
