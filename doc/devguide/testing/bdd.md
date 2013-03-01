### BDD integration tests

The BDD (Behaviour-Driven Development) tests reside under the `BDD/` directory, and should focus on UI/API controller and view testing.  They complement the [unit tests](../testing.md) which focus on model and non-visible controller tests.

The BDD tests are written in a "Cucumber-like" domain specific language (DSL), and the testing framework is written in Erlang.

#### Running the tests via the dev tool

[Running the tests via the dev tool](devtool.md) is the recommended approach.

#### Running the tests outside the dev tool

*NOTE:* Unfortunately the BDD tests (written in Erlang) currently do not work
on openSUSE yet.

   ````
   cd /tmp/crowbar-dev-test/opt/dell/crowbar_framework/BDD
   ./linux_compile.sh
   ./linux_run.sh
   ````

Note that the BDD tests require a running instance of Crowbar, which is
started by `linux_run.sh`.  If it fails with an error message like:

   ````
   ERROR: step run found error:{badmatch,{error,econnrefused}}
   ...
   ````

this means that the Crowbar server is not running and can usually be fixed
by running `linux_run.sh` again.

Once the server is running on `localhost`, you can run the BDD tests interactively from erlang (`erl`) in from the code area (not just the test area) using commands like `bdd:test(crowbar).` and `bdd:debug(crowbar, myfeature, mytestid, debug).`  
