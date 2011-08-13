
name "cloudfoundry-server"
description "Cloud Foundry Servier Role - OpenPaaS for the cloud"
run_list(
         "recipe[cloudfoundry::api]",
         "recipe[cloudfoundry::monitor]"
)
default_attributes()
override_attributes()

