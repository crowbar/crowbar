## Testing Crowbar

The Dell Crowbar team believes in testing!  We want you to write tests for your code and include it in your pull requests.

We have several mechanisms for testing and the following sections will help you understand them, use them and love them.

Our testing patterns include:

* DevTool Testing - integrates testing for developers
* Unit Tests - these these validate core logic assumptions in the models and controllers
   * these tests are written in Ruby and are part of the Rails framework
   * these tests are tightly coupled to the code of the system
   * these can be RSpec or Test::Unit based
   * RSpec will be the preferred method but other forms will be run.
* Business Driven Tests (BDD) - these are system integration tests that exercise the web UI without any integration to the code
   * these tests are written in a "Cucumber-like" domain specific langugage (DSL)
   * the testing framework (BDD) is written in Erlang

### Tips & Tricks

#### Dashboard No Polling

When you are troubleshooting the UI or REST APIs, the Node Dashboard (`dashboard`) polling can be a pain because it generates log traffic.  You can disable polling for debug by using the `nopoll` parameter.

For example, `http://192.168.124.10:3000/dashboard/89?nopoll`
