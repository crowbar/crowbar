name "health_manager"
description "Cloudfoundry health manager"
run_list("recipe[essentials]",
         "recipe[ruby]",
         "recipe[health_manager]"
)
