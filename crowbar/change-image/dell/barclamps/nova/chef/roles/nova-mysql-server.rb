name "nova-mysql-server"
description "mysql server setup for nova"

run_list(
  "recipe[nova::mysql]",
  "recipe[nova::monitor]"
)
