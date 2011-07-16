
name "ganglia-client"
description "GANGLIA Client Role - Nodes in the environment that should be monitored"
run_list(
   "recipe[ganglia::client]"
)
default_attributes()
override_attributes()
