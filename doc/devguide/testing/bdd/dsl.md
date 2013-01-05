### BDD Domain Specific Language (DSL)

The BDD DSL is designed to be very natural language like.  There are 4 primary clauses in the DSL:

1. General Purpose Steps
  1. **Given** ... some background thing has happened
  1. **When** ... take some action
  1. **Then** ... get some result
1. Special Purpose Steps;
  1. **Skip** ... will disable the test (please provide a reason after the skip)
  1. **Unless** environment ... will only run the test in the environments included after the Unless
  1. **Finally** ... cleanup actions (optional)

Feature files may also include **setup** and **tear down** steps that are essential for creating input data for tests.  In some cases, tests require information to be in place _before_ the Given step.

### Writing Feature Tests

Test files all end with the extension `.feature` and contain "plain English" scripts for testing features.  This is known as the BDD DSL.  While it looks like plain language, it is very specifically mapped into the testing framework and _must_ follow the DSL guidelines.

A feature file (in the `features` directory) is broken into specific "scenarios" to be tested.  Each scenario is effectively a test and has multiple steps.  They all start with a known state expressed using given or when instructions.  The state is then tested using then checks.  The concept is to mirror actions that a user takes: when the user takes this action then they should see these results.  Yes, it's that simple!

A scenario must include a when statement but the given statement is optional.  Given is used to setup a scenario before the when action is taken.  This is very important for testing linking from a page.  For example, _given_ that I'm on the nodes list page _when_ I click on the all link _then_ I should get a list that includes the admin node.  BDD's goal is to turn those types of directives into tests.

#### HTML Tests

The following sentences can be used for testing HTML web pages where you can change the information in "quotes".

* Given I am on the home page
* Given I am on the "dashboard" page
* Given there is a node "foo.example.com"
* When I go to the home page
* When I go to the "node/2.0/1" page
* When I click on the "Dashboard" menu item
* Then I should see "Full Name"
* Then there should be no translation errors
* Then I should not see "Error"
* Finally throw away node "foo.example.com"

> Note: This is _not_ a complete list!  To get a complete list of the tests use the `bdd:steps().` command.


#### REST/AJAX Tests

The following sentences can be used for testing REST JSON (aka AJAX) API calls where you can change the information in 

* When REST requests the "2.0/node/status" page
* Then key "fingerprint" should be a number
* Then key "[nodes][admin][state]" should be "Ready"
* Then key "count" should be "0"
* Then key "[groups][0]" should contain "7" items
* Finally throw away node "foo.bar.com"

> Note: This is _not_ a complete list!

> Note: We are migrating to use "REST" instead of "AJAX"
