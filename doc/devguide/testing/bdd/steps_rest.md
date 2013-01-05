#### BDD testing RESTful APIs

This section descusses the common pattern for BDD testing a RESTful API


##### Exports

* step/3
* json/3 - 
* validate/1
* inspector/1
* g/1

#### g

Put commonly used string and information here.  This function is provided to help avoid duplication of strings.  It makes it much easier to maintain the code if you centralize the string table.

* path - provides the path to the REST API that is the base for this object.  Commonly used from other modules.
* name & atom - are used as a string table for objects created in the steps
* type (not required) - is shown as a reference for information that you may want to keep in g

    g(Item) ->
      case Item of
        path -> "2.0/crowbar/2.0/cmdb";
        type -> "CmdbTest";
        name1 -> "bddcmdb";
        atom1 -> cmdb1;
        _ -> crowbar:g(Item)
      end.

#### json & validate

A Happy API has consistent JSON.

The json routine creates a valid JSON version of the object.  This is used by the routines that create and update the object via the API.  You may implement multiple versions of json to capture the different required/optional components of the object.

  json(Name) ->
    json:output([{"name",Name}]).

  json(Name, Description, Order) ->
    json:output([{"name",Name},{"description", Description}, {"order", Order}]).

Use the shared validator to check common properties like ID, Name, Description and edit dates.  This validator should only check the items that are specific to your object.

> The `bdd_utils:is_a` is your friend - extend it if needed.  There are similar BIFs for erlang that you can also leverage.

  validate(J) ->
    R =[length(J) =:= 6,
        bdd_utils:is_a(J, str, description), 
        bdd_utils:is_a(J, integer, order),
        crowbar_rest:validate(J)],
    bdd_utils:assert(R). 
         
#### inspector

The inspector is part of a housekeeping system for BDD that helps detect orphaned artifacts.  It is optional, but recommended for root objects.

The objective of the inspector method is to return a list of items found in the system.  This list is generated before and after BDD runs.

    inspector(Config) -> 
      crowbar_rest:inspector(Config, cmdbs).  % shared inspector works here, but may not always

> It is likely that you can leverage a generic inspector routine if your API has a consistent list pattern.

#### step - setup & teardown

Setup and teardown are called automatically by BDD at the start of a feature run.  They are optional.  It is import that they are balanced - any objects created by setup should be removed in teardown.

Setup and teardown steps are just like other steps except that they use the `step_setup` and `step_teardown` atoms.  Setup is expected to return a Config file.  Generally, a tumple for each object created (`{name1, 5}`) is added to the config so that teardown can delete objects by ID instead of name.

Setup

    step(Config, _Global, {step_setup, _N, _}) -> 
      % create node(s) for tests
      Node = json(g(name), g(description), 100),
      crowbar_rest:create(Config, g(path), g(atom), g(name), Node);

Teardown
    
    step(Config, _Global, {step_teardown, _N, _}) -> 
      % find the node from setup and remove it
      crowbar_rest:destroy(Config, g(path), g(atom)).
    
#### Common steps

Common steps are easy to create because thy can leverage existing steps with minor changes.  Even if the underlying step is simple, it's more maintainable to build steps based on other steps.

Get List 

    step(Config, _Given, {step_when, _N, ["REST gets the cmdb list"]}) -> 
      bdd_restrat:step(Config, _Given, {step_when, _N, ["REST requests the",eurl:path(g(path),""),"page"]});

Get Object

    step(Config, _Given, {step_when, _N, ["REST gets the cmdb",Name]}) -> 
      bdd_restrat:step(Config, _Given, {step_when, _N, ["REST requests the",eurl:path(g(path),Name),"page"]});

Validate Object

> This routine will call back the the modules own validate!

    step(_Config, Result, {step_then, _N, ["the cmdb is properly formatted"]}) -> 
      crowbar_rest:step(_Config, Result, {step_then, _N, ["the", cmdb, "object is properly formatted"]});


Create Object

>Creates a new object using the require components.  The routine builds the JSON for the object (see above) and then calls the shared create method.

    step(Config, _Global, {step_given, _N, ["there is a cmdb",CMDB,"of type", Type]}) -> 
      JSON = json(CMDB, g(description), Type, 200),
      crowbar_rest:create(Config, g(path), JSON);

Remove Object

    step(Config, _Given, {step_finally, _N, ["REST removes the cmdb",CMDB]}) -> 
      crowbar_rest:destroy(Config, g(path), CMDB);

#### Reference Features

    Scenario: CMDB List
      Given there is a cmdb "my_special_cmdb"
      When REST gets the cmdb list
      Then there should be a value "my_special_cmdb"
        And there should be a value "chef"
        And there should be a value "bddcmdb"
      Finally REST removes the cmdb "my_special_cmdb"
  
    Scenario: REST JSON check
      Given there is a cmdb "cmdb_json_test"
      When REST gets the cmdb "cmdb_json_test"
      Then the cmdb is properly formatted
      Finally REST removes the cmdb "cmdb_json_test"
  
    Scenario: REST Add 
      Given there is not a cmdb "cmdb_add_test"
      When REST adds the cmdb "cmdb_add_test"
      Then there is a cmdb "cmdb_add_test"
      Finally REST removes the cmdb "cmdb_add_test"
  
    Scenario: REST Delete 
      Given there is a cmdb "cmdb_delete_test"
      When REST deletes the cmdb "cmdb_delete_test"
      Then there is a not cmdb "cmdb_delete_test"

