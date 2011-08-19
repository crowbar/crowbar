name "cloud_controller"
default_attributes()
override_attributes()
description "Cloudfoundry cloud controller"
run_list("recipe[essentials]",
         "recipe[ruby]",
         "recipe[postgresql]",
         "recipe[mysql::server]",
         "recipe[mysql]",
         "recipe[cloud_controller]"
)
