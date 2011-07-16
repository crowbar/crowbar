
name "nagios-client"
description "NAGIOS Client Role - Nodes in the environment that should be monitored"
run_list(
   "recipe[nagios::client]"
)
default_attributes()
override_attributes()
