name "nova-multi-compute"

description "Installs requirements to run a Compute node in a Nova cluster"
run_list(
         "recipe[nova::compute]",
         "recipe[nova::monitor]"
         )
