
name "crowbar"
description "Crowbar role - Setups the rails app"
run_list(
         "recipe[utils]",
         "recipe[sudo]",
         "recipe[crowbar]"
)
default_attributes(
  :crowbar => { :admin_node => true },
  :rails => { :max_pool_size => 256, :environment => "production" },
  :passenger => { :max_pool_size => 256 },
  :authorization => {
    :sudo => {
      :groups => [ "admin" ],
      :users => [ "crowbar" ],
      :passwordless => true
    }
  }
)
override_attributes()

