         

name "ubuntu_install"
description "Ubuntu install server side setup"  
run_list(         
         "recipe[ubuntu-install::server]"
)