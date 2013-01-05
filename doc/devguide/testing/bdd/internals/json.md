#### JSON Parser

The BDD JSON parser is designed as a stand-alone JSON parser.  It is used to support REST API calls

The JSON parser turns JSON (`{Key1:Value1, Key2:Value2}`)into an Erlang Tuple List (`[{Key1, Value1}, {Key2, Value2}]`) where each key-value pair becomes an Erlang tuple.  If the JSON is nested, then the Erlang will also nest the JSON.

> Arrays & the JSON parser.  The parser converts JSON Arrays into Hash where the key values are a numbered index.  For example, `{Key:[Arr1, Arr2, Arr3]}` becomes `[{Key, [{0, Arr1}, {1, Arr2}, {2, Arr3}]}]`.

##### Records
The JSON parser uses Erlang records to pass data between the recursive routines.

* json is used by the top level parser
* jsonkv is used by the value subparser

##### Keyfind & Value List
Keyfind is a helper to make DRY the lists:keyfind code.  This is a very simple, 1st order search.

Value is similar to keyfind, but performs a more exhaustive search of the JSON

##### Parser Workers

* parse calls the json parser functions
* json is the primary worker that identifies keys and then uses json_value to resolve matching values
* json_value uses the : as a token and resolves/recurses to retrieve the value for a key
* json_array is similar to json_value but handles the [] array values
* json_value_quoted is used to find values inside of quotes

##### Pretty 
Formats the JSON parse output into a human readable, intended format

##### Output 
Turns the erlang list from the parser back into JSON
