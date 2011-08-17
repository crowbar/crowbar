         

name "redhat_install"
description "Redhat install server side setup"  
run_list(         
         "recipe[redhat-install::server]"
)