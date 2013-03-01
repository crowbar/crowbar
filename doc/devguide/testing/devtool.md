### Dev Tool Helpers 

The dev tool allows you to set up a test environment and run tests
from it.

Firstly ensure you have [set up a development VM](../dev-vm.md) which has
the correct dependencies installed.

The following commands work for UNIX environments.  When you first run
`./dev tests run`, use the `--update-gem-cache` option to make sure you
have the gems.

#### Setting up the test environment

`./dev tests setup` builds a test environment in `/tmp/crowbar-dev-test`.
On openSUSE, you should use the `--no-gem-cache` option.

Setup includes the following tasks:

* copying the sources into the right locations under
  `/tmp/crowbar-dev-test/opt/dell`
* running bundler to install required gems
* creating a `coverage` symlink under the Rails app's `public/`
  directory so that the web server will serve up the HTML coverage
  reports
* compiling the BDD tests

#### Running the tests

    ./dev tests run       # executes the Rails specs, Chef specs, unit tests, and BDD tests

You can also run them with finer granularity:

    ./dev tests run-unit  # executes the Rails specs, Chef specs, and unit tests
    ./dev tests run-BDD   # executes the BDD tests

After changing code, you can re-run the above command after first
rebuilding fixtures and performing any required data base migrations via:

    ./dev tests reload    # builds fixtures and migrates data for /tmp/crowbar-dev-test

For debugging, the `/tmp/crowbar-dev-test/opt/dell/crowbar_framework`
environment can be used as a Rails app for [manual UI testing](web-ui.md).

Note: only the `/tmp/crowbar-dev-test` area will have all the tests available because of the barclamp rollup.

#### Cleaning up

    ./dev tests clear     # Removes the /tmp/crowbar-dev-test environment

#### More information

See the [Testing page](../testing.md) for more information on each set of tests.
