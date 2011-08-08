

name "ubuntu_base"
description "Base setup for Ubuntu installs"
run_list(
         "recipe[ubuntu-install::apt_repo]",
         "recipe[ubuntu-install::base]",
         "recipe[apt]"
)

