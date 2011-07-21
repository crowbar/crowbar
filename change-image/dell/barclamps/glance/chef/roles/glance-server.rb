
name "glance-server"
description "Glance Servier Role - Glance for the cloud"
run_list(
         "recipe[glance::api]",
         "recipe[glance::registry]",
         "recipe[glance::monitor]"
)
default_attributes()
override_attributes()

