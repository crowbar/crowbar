
action :run do
  name = new_resource.name
  settle_time = new_resource.settle_time

  # Make sure the IPMI kernel modules are installed
  bash "install-ipmi_si" do
    code "/sbin/modprobe ipmi_si"
    not_if { ::File.exists?("/sys/module/ipmi_si") }
    returns [0,1]
    ignore_failure true
  end
 
  bash "install-devintf" do
    code "/sbin/modprobe ipmi_devintf"
    not_if { ::File.exists?("/sys/module/ipmi_devintf") }
    returns [0,1]
    ignore_failure true
  end

  bash "settle ipmi load" do
    code "sleep #{settle_time}"
    not_if { ::File.exists?("/sys/module/ipmi_devintf") and ::File.exists?("/sys/module/ipmi_si") }
  end
end

