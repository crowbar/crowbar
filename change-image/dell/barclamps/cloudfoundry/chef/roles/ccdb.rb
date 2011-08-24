name "ccdb"
default_attributes()
override_attributes()
description "Cloudfoundry cloud controller database"
run_list("recipe[essentials]",
         "recipe[mysql::server]",
         "recipe[mysql]"
)
