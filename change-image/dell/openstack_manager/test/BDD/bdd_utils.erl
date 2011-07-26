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
-module(bdd_utils).
-export([assert/1, assert/2, config/2, tokenize/1, clean_line/1, strip_doctype/1, uri/2]).
-export([http_get/2, http_get/3, html_peek/2, html_search/2, html_search/3, html_find_link/2, debug/3, debug/2, debug/1, trace/6]).
-export([http_post_params/1, http_post/4]).

assert(Bools) ->
	assert(Bools, true).
assert(Bools, Test) ->
	F = fun(X) -> case X of Test -> true; _ -> false end end,
	lists:any(F, Bools).
	
debug(Format) -> debug(Format, []).
debug(true, Format) -> debug(true, Format, []);
debug(false, Format) -> debug(false, Format, []);
debug(Format, Data) -> debug(false, Format, Data).
debug(Show, Format, Data) ->
  case Show of
    true -> io:format("DEBUG: " ++ Format, Data);
    _ -> noop
  end.

% Return the file name for the test.  
trace_setup(Config, Name, nil) ->
  trace_setup(Config, Name, 0);

trace_setup(Config, Name, N) ->
  SafeName = clean_line(Name),
  string:join(["trace_", config(Config,feature), "-", string:join(string:tokens(SafeName, " "), "_"), "-", integer_to_list(N), ".txt"], "").
  
trace(Config, Name, N, Steps, Given, When) ->
  File = trace_setup(Config, Name, N),
  {ok, S} = file:open(File, write),
  lists:foreach(fun(X) -> io:format(S, "~n==== Step ====~n~s", [X]) end, Steps),
  lists:foreach(fun(X) -> io:format(S, "~n==== Given ====~n~s", [X]) end, Given),
  lists:foreach(fun(X) -> io:format(S, "~n==== When ====~n~s", [X]) end, When),
  io:format(S, "~n==== End of Test Dump (~p) ====", [N]),
  file:close(S).
 
html_search(Match, Results, Test) ->
	assert([html_peek(Match,Result) || Result <- Results, Result =/= [no_op]], Test).
html_search(Match, Results) ->
	html_search(Match, Results, true).

strip_doctype(Input) ->
  RegEx = "<!DOCTYPE(.*)>",
	{ok, RE} = re:compile(RegEx),
	case re:run(Input, RE) of
		{match, S} -> [{Start, Length} | _] = S, 		% we only want the 1st expression!
					  string:substr(Input, Start+1+Length);
		_ -> Input
	end.
  
html_peek(Match, Input) ->
  RegEx = "<(html|HTML)(.*)"++Match++"(.*)</(html|HTML)>",
	{ok, RE} = re:compile(RegEx, [caseless, multiline, dotall, {newline , anycrlf}]),
	bdd_utils:debug("html_peek compile: ~p on ~p~n", [RegEx, Input]),
	Result = re:run(Input, RE),
	bdd_utils:debug("html_peek match: ~p~n", [Result]),
	%{ match, [ {_St, _Ln} | _ ] } = Result,
	%bdd_utils:debug("html_peek substr: ~p~n", [string:substr(Input, _St, _Ln)]),
	case Result of
		{match, _} -> true;
		_ -> Result
	end.
	
% return the HREF part of an anchor tag given the content of the link
html_find_link(Match, Input) ->
	RegEx = "(\\<(a|A)\\b(/?[^\\>]+)\\>"++Match++"\\<\\/(a|A)\\>)",
	{ok, RE} = re:compile(RegEx, [multiline, dotall, {newline , anycrlf}]),
	AnchorTag = case re:run(Input, RE) of
	  {match, [{AStart, ALength} | _]} -> string:substr(Input, AStart+1,ALength);
	  {_, _} -> io:format("ERROR: Could not find Anchor tags enclosing '~p'.  HTML could have other components encoded in a tag~n", [Match]), throw("could not html_find_link")
	end,
	{ok, HrefREX} = re:compile("\\bhref=(['\"])([^\\s]+?)(\\1)", [multiline, dotall, {newline , anycrlf}]),
	Href = case re:run(AnchorTag, HrefREX) of
	  {match, [_1, _2, {HStart, HLength} | _]} -> string:substr(AnchorTag, HStart+1,HLength);
	  {_, _} -> io:format("ERROR: Could not find href= information in substring '~p'~n", [AnchorTag]), throw("could not html_find_link")
	end,
	bdd_utils:debug("html_find_link anchor ~p~n", [AnchorTag]),
	%bdd_utils:debug(, "html_find_link href regex~p~n", [re:run(AnchorTag, HrefREX)]),
	bdd_utils:debug("html_find_link found path ~p~n", [Href]),
	Href.

uri(Config, Path) ->
	{url, Base} = lists:keyfind(url,1,Config),
  case {string:right(Base,1),string:left(Path,1)} of
    {"/", "/"}-> Base ++ string:substr(Path,2);
    {_, "/"}  -> Base ++ Path;
    {"/", _}  -> Base ++ Path;
    {_, _}    -> Base ++ "/" ++ Path
  end.
  
% get a page from a server
http_get(Config, Page) ->
	http_get(Config, Page, ok).
http_get(Config, Page, not_found) ->
	http_get(uri(Config,Page), 404, "Not Found");
http_get(Config, Page, ok) ->
	http_get(uri(Config,Page), 200, "OK(.*)");
http_get(URL, ReturnCode, StateRegEx) ->
	{ok, {{"HTTP/1.1",ReturnCode,State}, _Head, Body}} = http:request(URL),
	{ok, StateMP} = re:compile(StateRegEx),
	%bdd_utils:debug(true, "hppt_get has: URL ~p = ~s~n", [URL, Body]),
	case re:run(State, StateMP) of
		{match, _} -> Body;
		_ -> "ERROR, return of " ++ URL ++ " result was not 200 OK"
	end.

http_post_params(ParamsIn) -> http_post_params(ParamsIn, []).
http_post_params([], Params) -> Params;
http_post_params([{K, V} | P], ParamsOrig) -> 
  ParamsAdd = case ParamsOrig of
    [] -> "?"++K++"="++V;
    _ -> "&"++K++"="++V
  end,
  http_post_params(P, ParamsOrig++ParamsAdd).

http_post(URL, Parameters, ReturnCode, StateRegEx) ->
  Post = URL ++ http_post_params(Parameters),
  {ok, {{"HTTP/1.1",ReturnCode, State}, _Head, Body}} = http:post(post, Post, [], []),
 	{ok, StateMP} = re:compile(StateRegEx),
	case re:run(State, StateMP) of
		{match, _} -> Body;
		_ -> "ERROR, return of " ++ URL ++ " result was not 200 OK"
	end. 

config(Config, Key) ->
	{Key, Value} = lists:keyfind(Key,1,Config),
	Value.
	

clean_line(Raw) ->
	CleanLine0 = string:strip(Raw),
	CleanLine1 = string:strip(CleanLine0, left, $\t),
	CleanLine11 = string:strip(CleanLine1, right, $\r),
	CleanLine2 = string:strip(CleanLine11),
	string:strip(CleanLine2, right, $.).

tokenize(Step) ->
	Tokens = string:tokens(Step,"\""),
	[string:strip(X) || X<- Tokens].
