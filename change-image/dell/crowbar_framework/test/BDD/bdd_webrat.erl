% Copyright 2011, Dell 
% 
% Licensed under the Apache License, Version 2.0 (the "License"); 
% you may not use this file except in compliance with the License. 
% You may obtain a copy of the License at 
% 
%  http://www.apache.org/licenses/LICENSE-2.0 
% 
% Unless required by applicable law or agreed to in writing, software 
% distributed under the License is distributed on an "AS IS" BASIS, 
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
% See the License for the specific language governing permissions and 
% limitations under the License. 
% 
% Author: RobHirschfeld 
% 
-module(bdd_webrat).
-export([step/3]).
-import(bdd_util).

step(_Config, _Global, {step_given, _N, ["I am on the home page"]}) -> 
	bdd_utils:debug("start from home.~n"), 
	bdd_utils:http_get(_Config, []);

step(_Config, _Global, {step_given, _N, ["I went to the", Page, "page"]}) ->
	bdd_utils:http_get(_Config, Page);
	
step(_Config, _Given, {step_when, _N, ["I go to the home page"]}) -> 
	bdd_utils:debug("go home.~n"), 
	bdd_utils:http_get(_Config, []);

step(_Config, _Given, {step_when, _N, ["I go to the", Page, "page"]}) -> 
	bdd_utils:debug("go to the ~p page~n", [Page]), 
	bdd_utils:http_get(_Config, Page);

step(_Config, _Given, {step_when, _N, ["I try to go to the", Page, "page"]}) ->
	%bdd_utils:debug("expect FAIL when going to the ~p page~n", [Page]), 
	bdd_utils:http_get(_Config, Page, not_found);

step(_Config, _Given, {step_when, _N, ["I click on the",Link,"link"]}) -> 
  _Debug = false,
	[URL | _] = [bdd_utils:html_find_link(Link, HTML) || HTML <- (_Given), HTML =/= []],
	bdd_utils:debug(_Debug, "URL ~p~n",[URL]),
	Result = case URL of
		[] -> io:format("CANNOT FIND LINK ~s~n", [Link]), error;
		_ -> bdd_utils:http_get(_Config, URL, ok)
	end,
	bdd_utils:debug(_Debug, "when I click result ~p~n", [Result]),
	Result;

step(_Config, _Result, {step_then, _N, ["I should not see", Text]}) -> 
	bdd_utils:debug("step_then result ~p should NOT have ~p on the page~n", [_Result, Text]),
	bdd_utils:html_search(Text,_Result, false);
	
step(_Config, _Result, {step_then, _N, ["I should see", Text]}) -> 
	bdd_utils:debug("step_then result ~p should have ~p on the page~n", [_Result, Text]),
	bdd_utils:html_search(Text,_Result);



step(_Config, _Result, {_Type, _N, ["END OF WEBRAT"]}) ->
  false.