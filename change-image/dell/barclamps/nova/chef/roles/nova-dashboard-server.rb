name "nova-dashboard-server"

run_list(
    "recipe[apt]",
    "recipe[apache2]",
    "recipe[apache2::modwsgi]",
    "recipe[nova::dashboard]",
    "recipe[nova::monitor]"
)
