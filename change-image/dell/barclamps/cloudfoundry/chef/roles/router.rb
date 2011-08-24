name "router"
description "Cloudfoundry router"
run_list("recipe[essentials]",
                 "recipe[ruby]",
                 "recipe[nginx]",
                 "recipe[router]"
)
