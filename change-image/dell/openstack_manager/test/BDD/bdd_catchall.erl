-module(bdd_catchall).
-export([step/3]).
-import(bdd_util).

step(_Config, _Result, {step_given, _N, ["I do nothing to", Text]}) ->  Text;
step(_Config, _Result, {step_when, _N, ["I do nothing to", Text]}) ->  Text;
step(_Config, _Result, {step_then, _N, ["I always pass"]}) -> true;
step(_Config, _Result, {step_then, _N, ["I always fail"]}) -> false;

step(_Config, _Result, {step_given, _N, StepAction}) ->
	io:format("ADD MISSING GIVEN STEP: \"step(_Config, _Global, {step_given, _N, ~p}) -> false;\"~n", [StepAction]),
	false;

step(_Config, _Result, {step_when, _N, StepAction}) ->
	io:format("ADD MISSING WHEN STEP: \"step(_Config, _Given, {step_when, _N, ~p}) -> false;\"~n", [StepAction]),
	false;

step(_Config, _Result, {step_then, _N, StepAction}) ->
	io:format("ADD MISSING THEN STEP: \"step(_Config, _Result, {step_then, _N, ~p}) -> false;\"~n", [StepAction]),
	false;
	
step(_Config, _Result, {StepType, _N, StepAction}) ->
	io:format("UNKNOWN STEP TYPE: \"step(_Config, _, {~p, _N, ~p}) -> false;\"~n", [StepType, StepAction]),
	false;

step(_Config, _Result, StepTupple) ->
	io:format("INVALID STEP TUPPLE: Cannot resolve ~p~n", [StepTupple]),
	false.
