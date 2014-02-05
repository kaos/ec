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

%%-type text() :: [32..126].
text() -> list(range(32,126)).

%%-type newline() :: [10].

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

number_props()->
    ?SIZED(S, number_props(S, [])).

number_props(0, Acc) ->
    {lists:reverse(Acc), length(Acc)};
number_props(S, Acc) ->
    number_props(
      S - 1,
      [?LET(
          [K, V], [atom(), number()],
          io_lib:format("~p=~p~n", [K, V]))
       |Acc]).

prop_parse_number() ->
    ?FORALL(
       {C, L}, number_props(),
       begin
           S = lists:flatten(C),
           {ok, Props} = ec:parse(S),
           L = length(Props),
           [case Prop of
                {Key, Value}
                  when is_atom(Key),
                       is_number(Value) ->
                    ok
            end || Prop <- Props],
           true
       end).
