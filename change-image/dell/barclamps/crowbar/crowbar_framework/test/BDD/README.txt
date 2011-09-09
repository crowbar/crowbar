This is a Business Driven Development (BDD) implementation 

To use BDD, 
1) you must make the erlang files using the build script.
2) start the web site for the target (update bdd.config with the correct address!)
3) run erlang shell
4) bdd:test("crowbar").

notes: 
   * the feature list (crowbar.feature) contains the test instructions.
   * bdd.config has config information for the test framework
   * crowbar.erl contains code that is unique to this application
   * webrat.erl contains code that is general for all web applications

Change Log:

1/9: Initial Submission.  Pending build script.