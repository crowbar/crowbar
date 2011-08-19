name "dea"
description "Cloudfoundry DEA"
run_list("recipe[essentials]",
         "recipe[ruby]",
         "recipe[ruby::ruby18]",
         "recipe[dea]",
         "recipe[nodejs]",
         "recipe[erlang]",
         "recipe[java]"
)