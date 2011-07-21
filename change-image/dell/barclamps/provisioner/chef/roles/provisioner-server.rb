
name "provisioner-server"
description "Provisioner Server role - Apt and Networking"
run_list(
         "recipe[utils]", 
         "recipe[dhcp]", 
         "recipe[tftpd]", 
         "recipe[nfs-server]", 
         "recipe[ubuntu-install]" 
)
default_attributes()
override_attributes()

