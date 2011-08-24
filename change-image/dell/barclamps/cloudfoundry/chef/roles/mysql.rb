name "mysql"
description "mysql db for apps"
run_list("recipe[essentials]",
         "recipe[ruby]",
         "recipe[mysql::server]",
         "recipe[mysql]",
         "role[services]"
)                