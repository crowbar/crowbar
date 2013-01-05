define :==BC-MODEL==_service do

  ==BC-MODEL==_name="==BC-MODEL==-#{params[:name]}"

  service ==BC-MODEL==_name do
    if (platform?("ubuntu") && node.platform_version.to_f >= 10.04)
      restart_command "restart #{==BC-MODEL==_name}"
      stop_command "stop #{==BC-MODEL==_name}"
      start_command "start #{==BC-MODEL==_name}"
      status_command "status #{==BC-MODEL==_name} | cut -d' ' -f2 | cut -d'/' -f1 | grep start"
    end
    supports :status => true, :restart => true
    action [:enable, :start]
    subscribes :restart, resources(:template => node[:==BC-MODEL==][:config_file])
  end

end
