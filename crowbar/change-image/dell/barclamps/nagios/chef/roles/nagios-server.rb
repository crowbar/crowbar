
name "nagios-server"
description "NAGIOS Server Role - NAGIOS master for the cloud"
run_list(
  "recipe[nagios::server]"
)
default_attributes(
  "nagios" => {
    "server_auth_method" => "htauth"
  }
)
override_attributes()

