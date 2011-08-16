name "==BC-MODEL==-server"
description "==^BC-MODEL== Servier Role"
run_list(
         "recipe[==BC-MODEL==::api]",
         "recipe[==BC-MODEL==::monitor]"
)
default_attributes()
override_attributes()

