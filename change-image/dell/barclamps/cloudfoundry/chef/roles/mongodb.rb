name "mongodb"
description "Cloudfoundry MongoDB"
run_list("recipe[essentials]",
         "recipe[ruby]",
         "recipe[mongodb]",
         "role[services]"
)
