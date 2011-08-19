name "services"
description "Cloudfoundry services"
run_list("recipe[essentials]",
                 "recipe[ruby]",
                 "recipe[cloudfoundry]",
                 "recipe[services]"
)
