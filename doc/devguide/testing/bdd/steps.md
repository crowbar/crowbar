### Extending the DSL

Each feature definition can add it's own ERL step parser.  The BDD system will automatically search for a step definition based on the feature name.  It will also automatically search the "crowbar" and "webrat" steps.  

The webrat steps are designed to be general purpose Web access checks.  If you find yourself doing a routine HTTP or AJAX request then it likely belongs in the webrat file instead of your feature steps.

You can add custom step files from the config page.


#### Steps

All of the BDD tests decompose into the same Erlang method, known as the `step` method. The BDD test engine will search in multiple Erlang code files for steps in a specific order.  

   > The steps have been defined to ensure that the last file tried (`bdd_catchall`) contains steps that have been defined to match all possible cases.  If the final catchall step is reached, that step will generate a stub step that you can use to create a new step.

A step is a standard Erlang function with 3 parameters:

1. The BDD configuration file
1. The pass forward file that represents the accumulated output of previous steps
1. The DSL tuple populated by BDD as follows
   1. keyword (setup, teardown, given, when, or then)
   1. step number
   1. list of the DSL string tokenized by quotes

  > If you are terrified by the phrase "tokenized by quotes" then relax.  That just means that BDD turns your friendly `when I click on link "foo"` into an Erlang list that is super easy to parse: ["when I click on link", Foo].  

Let's look at an example step:

    step(Config, _Global, {step_given, {_ScenarioID, _N}, ["I went to the", Page, "page"]}) ->
        bdd_utils:http_get(Config, Page);

This step will match the DSL `Given I went to the "dashboard" page` in the scenario.  It simply does an HTTP get using the BDD utilities.  The `http_get` routine takes the base URL from the config file and adds the page information from the sentence.  BDD will take the result of this step function and add it to the `Given` list that is passed into all following 'when' steps.

  > You can also use `{` and `}` for delimiters.  Generally, these delimiters are used for special items like class types.
  > Reminder: Erlang variables that start with "\_" are considered optional and don't throw a warning if they are not used.  If you plan to use those variables, you can keep the "\_", however, I recommend removing it for clarity.

There is a simple output expectation from all steps:

* setup steps add to the Global list that is passed into Given steps
* given steps add to the Given list that is passed into When steps
* when steps add to the Results list that is passed into the Results steps
* results steps return true if the test passes or something else if it fails

One of the most important step files is known as "webrat" as a hold over from Cucumber.  The `bdd_webrat.erl` file contains most of the HTML & AJAX routines you will ever need for routine testing.  It is also a great place to look for examples of step programming.

You can extend/substitute step definitions by using the one of the predefinex prefixs.  This will invoke an Erlang method when the step is tokenized so that you can create custom lookups for your steps.

For example, using `{bdd:bdd_utils.puts.foo.bar}` will call the `Bdd_utils` method `puts/2` with the input data of `["foo","bar"]`.  This approach can be used to extend steps so that they can substitue information when the step is parsed.  This is NOT a test run substitution - the replacement is made before test execution.

> Remember, prefixes are static data used during _pre-evaluation_ for steps.  You cannot use prefixs to lookup information that is only available during testing such as the IDs of created steps.  Use the `bdd_utils:scenario_retrieve` methods inside the steps if you need dynamic information.

BDD step prefixes are:

* bdd: does an apply() using the BDD Config pattern and `.` delimited information where `bdd:f.m.p1.p2.pN` becomes `apply(f, m, [Config, p1, p2, pN])`
* apply: does a raw apply() using `.` delimited information where `apply:f.m.p1.p2.pN` becomes `apply(f, m, [p1, p2, pN])`
* lookup: calls the g(type) value for the given model.  Use as `lookup:node.name` to call the `node.erl g(name)` method.  This is a very handy way to use the items created in the setup steps.
* atom: turns the text into an Erlang atom
* integer: turns the text into an Erlang integer
* object: same as atom but more user intuitive for REST steps

> A handy example of this for Crowbar is using the `crowbar:i18n(Config, Key)` lookup to resolve localizations using the Utils localization string retriever.  You can retrieve localizations in steps using `{bdd:crowbar.i18n.my.local.key}` where `my.local.key` maps to a key in the i18n files.

#### Adding Pre & Post Conditions

To add pre/post-configuration for a Feature file, you must have an Erlang step file with the same name as the feature file.  For example, if you have a feature called `nodes.feature` then you must have a `nodes.erl` to create setup and tear down steps for that feature file.

Setup Steps use the `step_setup` atom:

    step(Config, _Global, {step_setup, {_ScenarioID, _N}, _}) -> 
      io:format("\tNo Feature Setup Step.~n"),
      Config;

> This setup step adds results to the Config file.  You should use `[{item, value} | Config]` to ensure that your values get added to the Config list and are available for the features' steps.
> _Global is always an empty list (`[]`) for setup steps.

Teardown Steps use the `step_teardown` atom:

    step(Config, _Global, {step_teardown, {_ScenarioID, _N}, _}) -> 
      io:format("\tNo Feature Tear Down Step.~n"),
      Config;

To perform actions, replace or augment the code in the steps to perform the needed operations.  The result from the Setup action is added to the `Global` list that is passed into all the steps called within the feature.  This allows you to reference items created in setup during subsequent tests.  You should remember to unwind any action from the setup in the teardown.

For example, the Nodes feature setup and tear down look like this:

    step(Config, _Global, {step_setup, {_ScenarioID, _N}, _}) -> 
      Path = "node/2.0",
      Node1 = "BDD1.example.com",
      % just in case, cleanup first
      http_delete(Config, Path, Node),
      % create node(s) for tests
      Node = node_json(Node1, "BDD Testing Only", 100),
      Result = http_post(Config, Path, Node),
      {"id", Key} = lists:keyfind("id",1,Result),
      io:format("\tCreated Node ~p (id=~p) for testing.~n", [Node1, Key]),
      [{node1, Key} | Config];
    
    step(Config, Global, {step_teardown, {_ScenarioID, _N}, _}) -> 
      % find the node from setup and remove it
      {"node1", Key} = lists:keyfind("node1", Global),
      http_delete(Config, Path, Key),
      io:format("\tRemoved Node ID ~p for Tear Down Step.~n", [Key]),
      Config;

### Passing information between Steps in a Scenario

There are two ways to pass information between Steps in a Scenario: the the Results list and the Config Bag.

#### The Results List

The Results list is the primary way that information to be tested is added/checked in steps.  The entire design of the BDD system is to have the When steps collect information that is checked by the Then steps.  The "Result" list is the way this information is passed into the Results for checking.  The same mechanism is used for passing information from the Given to When steps.

#### The Config Bag

The Results List is the primary mechanism to use this because the trace and step designs encourage it; however, there are times when it is necessary to collect Given information and use it in Then steps.  In these cases, you can store Scenario specific information for use within the other steps using Scenario store and retrieve routines.

To Store, use `bdd_utils:scenario_store(ScenarioID, Key, Value)` in insert a KVP into the configuration thread space.  These values can be inspected from the Erlang shell using the regular `get({scenario, ID#})` approach.  It is recommended to store IDs of objects that Given steps create so they are avialable during Then checks.

> Storing the same key again will replace the old value with the new value.

To Revieve the value later, use `bdd_utils:scenario_retrieve(ScenarioID, Key, Default).`  This will recover the key value for the Scenario.

This approach is very handy: 
* if you want to store given values for a form (`Given I set "foo" to "bar"` - see `bdd_catchall`).
* if you create value (`Given REST creates {object:node} "foo.example.com"`) and need it's ID for a later step

### Debugging

Some handy Erlang tips:

* `Config = bdd:getconfig("crowbar")` will load the configuration file for passing into Step routines for manual testing

### BDD Code Files

* bdd.erl - contains the core running logic
* bdd_utils.erl - utilities used across all modules of the bdd system
* eurl.erl - HTTP get, post, delete functions (like curl)
* json.erl - JSON parser converts to and from lists
* digest_auth.erl - Wrapps http to provide secure access
* bdd_catchall.erl - last step file executed, has fall back steps
* bdd_webrat.erl - handles most basic web & AJAX based steps
* default.erl - the fall back step file (global setup/teardown goes here)
* crowbar.erl - Crowbar specific logic
* [feature].erl - Each feature can have a specific step file

#### In the feature specific code files, you will find the following

#### The Global routine "g" 
Provides paths for the Feature type.  Using g helps to make the code DRY.

It is common for other features to call each other's g routines to get the correct path for operations on that type.

#### JSON creator & validator
The json method is used to create json text for POST and PUT operations against the API.
The validate method is used to make sure that GET returned json matches the expected results

#### Inspector
The inspector inspects the system and returns a list of items that reflect it's current state.  The goal of the inspector is to help detect testing artificats that should have been removed.  The inpector method is called before any tests are run and again after all the tests have completed.  If there is any new or missing artificat, the BDD inspector will alert you that the system was not left in a clean state.

#### Setup and Teardown steps for the Feature.
These steps are called by BDD before and after the feature are executed.  They create objects for the tests to manipulate and then restore the system to it's original state.

REST API items that are specific to that Feature; however, some of these are common and should be moved to the Crowbar or CrowbarREST file.
