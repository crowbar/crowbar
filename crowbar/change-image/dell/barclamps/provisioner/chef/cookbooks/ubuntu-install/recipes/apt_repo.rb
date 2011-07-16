
c = cookbook_file "/etc/apt/apt.conf" do
  source "apt.conf"
  owner "root"
  group "root"
  mode "0644"
  action :create
end

c.run_action(:create)

