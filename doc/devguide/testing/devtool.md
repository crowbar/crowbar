### Dev Tool Helpers 

The dev tool has been updated to allow for running these tests from in a build environment.  

The following gems are pre-requisite (must run as root)
* `sudo -i`
* `apt-get install sqlite3`
* `gem install rake`
* `gem install bundler`
* `gem install sqlite3`
* Run with `--update-gem-cache` on your first `run-unit-tests` to make sure you have the tests
The following commands work for Unix environments:

1. `./dev setup-unit-tests`   # builds a unit test environment in /tmp/crowbar-dev-test
1. `./dev reload-unit-tests`  # builds fixtures and migrates data for /tmp/crowbar-dev-test
1. `./dev run-unit-tests`     # executes the chef-spec, unit tests and BDD tests
1. `./dev clear-unit-tests`   # Removes the /tmp/crowbar-dev-test environment

For debugging, the `/tmp/crowbar-dev-test/crowbar_framework` environment can be used as a rails app.  

The crowbar webserver can be run by running:
* `bundle exec rails s`

You must do this if you want to debug the BDD tests!  Once the server is running on localhost, you can run the BDD tests interactively from erlang (`erl`) in from the code area (not just the test area) using commands like `bdd:test(crowbar).` and `bdd:debug(crowbar, myfeature, mytestid, debug).`  

> Note: only the `crowbar-dev-test` area will have all the tests available because of the barclamp rollup.

