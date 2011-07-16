name "nova-rabbitmq-server"

run_list(
    "recipe[rabbitmq]",
    "recipe[nova::rabbit]",
    "recipe[nova::monitor]"
)
