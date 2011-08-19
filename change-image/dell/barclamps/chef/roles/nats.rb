name "nats"
description "NATS message bus"
run_list("recipe[essentials]",
         "recipe[ruby]",
         "recipe[nats-server]" 
)