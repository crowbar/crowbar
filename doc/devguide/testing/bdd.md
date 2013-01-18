### BDD Testing

Crowbar includes a Business Driven Development (BDD) framework written in Erlang that is based on the Cucumber DSL (Domain Specific Language).

The intent of these tests are to focus on the responses and requests to the web-interface and RESTful API.

Core BDD Commands:
1. test - runs the complete suite
2. feature - runs a single feature test file
3. scenario - runs a single test from a feature file
4. steps - shows available steps already created when creating new scenarios

> The site you are trying to test MUST BE RUNNING!

### Using the BDD tool

1. `cd /opt/dell/crowbar_framework/BDD`
1. `linux.sh` or `Win7.bat` to compile the erlang code depending on your platform (may give an error, that's ok)
1. `erl` to start a command shell for erlang
  1. `bdd:test().` will run all the tests and report the results.  Test results are copied to a `../tmp/bbd_results.out` with a date/time stamp so you can review test status (see failed() below).
  1. `bdd:feature(name).` will run just the named feature set.  You can pass the feature name as an atom or string.
  1. `bdd:scenario(name, id).` will run just the scenario selected from the feature file.  ID's are assigned by BDD based on a hash of the scenario name.
  1. `bdd:debug(config, name, id).` will run just the scenario selected from the feature file with debug logging flags.  ID's are assigned by BDD based on a hash of the scenario name.  
     1. You may also pass a list of the specific log levels requested.  (if omitted, `debug` is assumed)
     1. You can pass a single atom instead of a whole list of log levels: `trace`, `debug`, and `info` are supported.
  1. `bdd:failed(config).` will rerun just the failed tests using the test results output file (`../tmp/bbd_results.out`).
  1. `bdd:steps().` will show you all the available step definitions

> You can run `bdd:test("profile").` or `bdd:feature("profile","feature").` if you want to use an alternate profile than `default`.  Alternate profiles use the matching configuration name and had a different global setup/teardown location.

> Note: The default tests run as the "developer" user so you must be in development mode to use them!

The BDD test results are reported using a condensed format with the feature name, total tests, passed tests, failed tests, skipped tests, and IDs of the failed tests.

#### Test Files

Each barclamp is expected to add it's own tests to the suite

The Crowbar barclamp tests include:

* dashboard.feature - Tests the nodes UI view
* documentation.feature - Tests the documentation/help system
* navigation.feature
   * Tests the basic menu system
   * Checks for localization omissions
* proposals.feature - Tests the Proposal Status API
* nodes.feature
   * Tests the node status API
   * Tests the node detail page & API
* groups.feature
   * Tests the group API
   * Tests the groups + nodes API
* scaffolds.feature - Tests all the feature objects
* authenticate.feature - Tests login
* users.feature - Tests user management screen
* attributes - Tests Jig attributes API
* jigs - Tests the Jig engine API
    

### Test Debugging

The BDD system generates trace files for each test executed.  These trace files have the results of all the steps for each scenario.  If the test passes, the trace file is deleted automatically. 

Reviewing the trace output on failed tests is the fastest way to determine if there is a problem with the system or the test because it will show you the page results that are being examined.

> Remember, if you change code then you must recompile (e.g.: `c(bdd).`) it!

#### Running BDD from Erlang
Erlang is a functional language, that means that you can run nearly any step if you can duplicate the input.  Nearly every BDD method requires the Config list.  The config list contains critical information about the environment and session data based on a system login.

To create a Config list, use the start command with a configuration: `bdd:start(default).`  This command will load the selected config, start the http & auth services and finally get a session for access to the web site.

> The session will expire if it is not used!  If the session expires, forget the values (`f(Cbase)` and `f(Config).`).

Once you have a valid Config list, there are wide range of options.  You can execute the global inspector using `bdd:inspect(Config).` or one in each feature using `node:inspector(Config).`

#### Interactive Debugger
To use the interactive debugger, you must 
1. compile the files using `show_debug` flag.  For example, `c(bdd, show_debug).`
1. start the debugger using `debugger:start().`
1. using the GUI to monitor the module and injection point desired

> The debugger is a little flaky.  Have patience!

#### BDD Inspector
Since BDD works against a live system without rollback, BDD has added checks to make sure that tests to not leave testing artificats in the database after a successful run.  

To implement this capability, each object related feature is expected to implement an `inspector` method that returns the current state of the objects that it will be acting on.  These routines are called before and after the tests are run.  If the list is different, then the BDD inspector will issue a warning and show the artifacts.

The pre-run artificat list is saved at `../tmp/inspection.list`

To retrieve the last inspector report, use `bdd:is_clean(Config).`

To generate the list used for the inspector report, use `bdd:inspect(Config).`
