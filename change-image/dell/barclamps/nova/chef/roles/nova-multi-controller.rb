name "nova-multi-controller"

description "Installs requirements to run the Controller node in a Nova cluster"
run_list(
         "role[nova-mysql-server]",
         "role[nova-rabbitmq-server]",
         "recipe[nova::api]",
         "recipe[nova::network]",
         "recipe[nova::objectstore]",
         "recipe[nova::scheduler]",
         "recipe[nova::project]",
         "recipe[nova::monitor]"
         )
