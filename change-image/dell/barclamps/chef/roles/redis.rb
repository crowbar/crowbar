name "Redis"
description "Redis key value store for apps"
run_list("recipe[essentials]",
         "recipe[ruby]",
         "recipe[redis]",
         "role[services]"

)