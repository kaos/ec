%%  
%%  Copyright 2014, Andreas Stenius <kaos@astekk.se>
%%  
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%  
%%     http://www.apache.org/licenses/LICENSE-2.0
%%  
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%  

-module(ec_props).

-include_lib("proper/include/proper.hrl").
%%% ----------------------------------------

%%-type text() :: [32..126].
text() -> list(range(32,126)).

%%-type newline() :: [10].

%%% ----------------------------------------
comments() ->
    ?SIZED(S, comments(S, [])).

comments(0, Acc) ->
    lists:reverse(Acc);
comments(S, Acc) ->
    comments(S - 1,
            [union(
               ["\n", %% newline(),
                ?LET(C, text(), "#" ++ C ++ "\n") %% newline()
               ])
             |Acc]
           ).

prop_parse_comments() ->
    ?FORALL(
       C, comments(),
       {ok, []} =:= ec:parse(lists:flatten(C))
      ).

%%% ----------------------------------------
prop_parse_key_value_pairs() ->
    ?FORALL(
       Ps, list({atom(), term()}),
       begin
           S = lists:flatten(
                 [io_lib:format("~p = ~w~n", [K, V])
                  || {K, V} <- Ps]),
           {ok, Ps} =:= ec:parse(S)
       end).

%%% ----------------------------------------
prop_parse_empty() ->
    ?FORALL(
       Ps, list(atom()),
       begin
           S = lists:flatten(
                 [io_lib:format("~p=~n", [K])
                  || K <- Ps]),
           {ok, Ps} =:= ec:parse(S)
       end).

%%% ----------------------------------------
prop_parse_object() ->
    ?FORALL(
       {Obj, Ps}, {atom(), list({atom(), term()})},
       begin
           P = [io_lib:format("\n\t~p = ~w", [K, V])
                || {K, V} <- Ps],
           S = lists:flatten(
                 io_lib:format(
                   "~p {~s~n}", [Obj, P])),
           {ok, [{Obj, Ps}]} =:= ec:parse(S)
       end).

%%% ----------------------------------------
prop_duplicate_keys() ->
    ?FORALL(
       {Key, Vs}, {atom(), list(term())},
       begin
           S = lists:flatten(
                 [io_lib:format("~p = ~w~n", [Key, V])
                  || V <- Vs]),
           {ok, [{Key, V} || V <- Vs]} =:= ec:parse(S)
       end).

%%% ----------------------------------------
%%% ----------------------------------------
