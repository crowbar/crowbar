
name "deployer-client"
description "Deployer Client role - Discovery components"
run_list(
         "recipe[ohai]",
         "recipe[barclamp]"
)
default_attributes()
override_attributes()

