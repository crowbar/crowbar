-module(bdd).
-export([test/1, test/3, feature/2]).  %this is the final one
-import(bdd_utils).

test(ConfigName) -> 
  test(ConfigName, search, []).
test(ConfigName, search, Tests) ->
  Config = getconfig(ConfigName),
  Features = filelib:wildcard("*." ++ bdd_utils:config(Config,extension)),
	application:start(inets),		% needed for getting we pages
  Results = [{feature, FileName, test(Config, FileName, Tests)} || FileName <- Features],
	application:stop(inets),
	Result = [R || {_, R} <- Results, R =/= pass],
	case Result of
		[] -> pass;
		_ -> bdd_selftest:test(all), Result
	end;
test(ConfigBase, FileName, Tests) -> 
	[Feature | _ ] = string:tokens(FileName,"."),
	Config = [{feature, Feature}, {file, FileName} | ConfigBase],		% stuff the file name into the config set for later
	{feature, Name, Scenarios} = feature_import(FileName),
	[ScenarioName, _ScenarioIn, _ScenarioWho, _ScenarioWhy | _ ] = [string:strip(S) || S <- Name, S =/= []],
	io:format(" FEATURE: ~s.~n", [ScenarioName]),
	{feature, ScenarioName, [setup_scenario(Config, Scenario, Tests) || Scenario <- Scenarios]}.

feature(ConfigName, FeatureName) ->
  Config = getconfig(ConfigName),
  FileName = FeatureName ++ "." ++ bdd_utils:config(Config,extension),
	application:start(inets),		% needed for getting we pages
  test(Config, FileName, []),
	application:stop(inets).
  
getconfig(ConfigName) ->
  {ok, ConfigBase} = file:consult(ConfigName++".config"),
  [{config, ConfigName} | ConfigBase].
  
% read in the feature file
feature_import(FileName) ->
	{ok, Features} = file:read_file(FileName),
	[Header | Body] = re:split(Features,"Scenario:"),
	Name = bdd_utils:clean_line(string:tokens(binary_to_list(Header),"\n")),
	Scenarios = [binary_to_list(S) || S <- Body],
	{feature, Name, Scenarios}.
	
% run the scenarios, test list allows you to pick which tests
setup_scenario(Config, Scenario, Tests) ->
	[RawName | RawSteps] = string:tokens(Scenario, "\n"),
	Name = bdd_utils:clean_line(RawName),
	Member = lists:member(Name,Tests),
	if 
	  Member; length(Tests) =:= 0 -> test_scenario(Config, RawSteps, Name);
	  true -> io:format("\tSKIPPED ~p~n", [Name])
	end.

% decompose each scearion into the phrases, must be executed in the right order (Given -> When -> Then)
test_scenario(Config, RawSteps, Name) ->
	{N, GivenSteps, WhenSteps, ThenSteps} = scenario_steps(RawSteps),
	% execute all the given steps & put their result into GIVEN
	bdd_utils:trace(Config, Name, N, RawSteps, ["pending..."], ["pending..."]),
	Given = [step_run(Config, [], GS) || GS <- GivenSteps],
	% now, excute the when steps & put the result into RESULT
	bdd_utils:trace(Config, Name, N, RawSteps, Given, ["pending..."]),
	When = case length(WhenSteps) of
	  0 -> Given;
	  _ -> [step_run(Config, Given, WS) || WS <- WhenSteps]
	end,
	bdd_utils:trace(Config, Name, N, RawSteps, Given, When),
	% now, check the results
	Result = [step_run(Config, When, TS) || TS <- ThenSteps],
	% now, run the then steps
	io:format("\tSTEP: ~p (~p steps) ", [Name, N]),
	case bdd_utils:assert(Result) of
		true -> io:format("PASSED!~n",[]), pass;
		_ -> io:format("~n\t\t*** FAIL (Result: ~p)***~n\t\tSTEPS:~n\t\t\tGIVEN: ~p~n\t\t\tWHEN: ~p~n\t\t\tTHEN: ~p~n", [Result, GivenSteps, WhenSteps, ThenSteps]), Name
	end.

% Inital request to run a step does not know where to look for the code, it will iterate until it finds the step match or fails
step_run(Config, Input, Step) ->
	StepFiles = [list_to_atom(bdd_utils:config(Config, feature)) | bdd_utils:config(Config, secondary_step_files)],
	step_run(Config, Input, Step, StepFiles).
% recursive attempts to run steps
step_run(Config, Input, Step, [Feature | Features]) ->
	try apply(Feature, step, [Config, Input, Step]) of
		error -> {error, Step};
		Result -> Result
	catch
		error: undef -> step_run(Config, Input, Step, Features);
		error: function_clause -> step_run(Config, Input, Step, Features);
		exit: {noproc, {gen_server, call, Details}} -> io:format("ERROR: web server not responding.  Details: ~p~n",[Details]), throw("BDD ERROR: Could not connect to web server.");
		X: Y -> io:format("ERROR: step run found ~p:~p~n", [X, Y]), throw("BDD ERROR: Unknown error type in BDD:step_run.")
	end;
% no more places to try, fail and tell the user to create the missing step
step_run(_Config, _Input, Step, []) ->
	io:format("ERROR: Unable to resolve step ~p!~n", [Step]),
	throw("FAIL: no matching expression found for Step"), 
	error.
	
% Split our steps into discrete types for sequential processing
% Each step is given a line number to help w/ debug
scenario_steps(Steps) ->
	%io:format("\t\tDEBUG: processing steps ~p~n", [Steps]),
	scenario_steps(Steps, 1, [], [], []).
scenario_steps([H | T], N, Given, When, Then) ->
	CleanStep = bdd_utils:clean_line(H),
	{Type, StepRaw} = step_type(CleanStep),
	Step = {Type, bdd_utils:tokenize(StepRaw)},
	bdd_utils:debug("line ~p~n", [Step]),
	case Step of
		{step_given, S} -> scenario_steps(T, N+1, [{step_given, N, S} | Given], When, Then);
		{step_when, S} -> scenario_steps(T, N+1, Given, [{step_when, N, S} | When], Then);
		{step_then, S} -> scenario_steps(T, N+1, Given, When, [{step_then, N, S} | Then]);
		{empty, _} -> scenario_steps(T, N, Given, When, Then);
		{unknown, Mystery} -> io:format("\t\tWARNING: No prefix match for ~p~n", [Mystery]), scenario_steps(T, N+1, Given, When, Then)		
	end;
scenario_steps([], N, Given, When, Then) ->
	% returns number of steps and breaks list into types, may be expanded for more times in the future!
	{N, Given, When, Then}.
	
% figure out what type of step we are doing (GIVEN, WHEN, THEN, etc), return value
step_type([$G, $i, $v, $e, $n, 32 | Given]) ->
	{ step_given, Given };
step_type([$W, $h, $e, $n, 32 | When] ) ->
	{ step_when, When };
step_type([$T, $h, $e, $n, 32 | Then ]) -> 
	{ step_then, Then };
step_type([]) ->
	{ empty, []};
step_type(Step) ->
	{ unknown, Step }.


