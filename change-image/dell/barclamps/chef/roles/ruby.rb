name "ruby"
description "Ruby"
run_list("recipe[essentials]",
                 "recipe[ruby::ruby18]",
                 "recipe[ruby]"
)
