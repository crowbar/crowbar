
apt_repository "local maverick main" do
  uri "file:///tftpboot/ubuntu_dvd"
  distribution "maverick"
  components [ "main", "restricted" ]
  action :add
end

apt_repository "local extra repos" do
  uri "file:///tftpboot/ubuntu_dvd/extra/"
  distribution "/"
  action :add
end

