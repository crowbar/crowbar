name "cloudfoundry"
default_attributes()
override_attributes()
description "Cloudfoundry components"
run_list("recipe[essentials]",
         "recipe[ruby]",
         "recipe[cloudfoundry]"
)
