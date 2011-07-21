name "nova-single-machine"
description "Installs everything required to run Nova on a single machine"
run_list(
         "role[nova-multi-controller]",
         "role[nova-multi-compute]",
         "recipe[nova::monitor]"
         )
