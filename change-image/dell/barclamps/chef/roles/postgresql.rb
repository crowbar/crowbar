name "Postgresql"
description "Postgresql db for apps"
run_list("recipe[essentials]",
         "recipe[ruby]",
         "recipe[postgresql]",
         "role[services]"              
)