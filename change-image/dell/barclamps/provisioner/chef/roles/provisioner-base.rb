
name "provisioner-base"
description "Provisioner Base role - Apt and Networking"
run_list(
         "recipe[ubuntu-install::apt_repo]", 
         "recipe[utils]", 
         "recipe[barclamp]", 
         "recipe[apt]", 
         "recipe[ubuntu-install::base]"
)
default_attributes()
override_attributes()

