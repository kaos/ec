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

-module(ec_parse).

-export([tokens/1]).

-include_lib("merl/include/merl.hrl").

-spec tokens(list()) -> {ok, list()} | {error, {erl_scan:line(), atom(), term()}}.
tokens(Ts) ->
    catch parse_tokens(Ts, []).

parse_tokens([], Ps) -> {ok, lists:reverse(Ps)};
parse_tokens([{expr, S}|Ts], Ps) ->
    parse_tokens(Ts, [normalise(S)|Ps]);
parse_tokens([{begin_object, S}|Ts0], Ps) ->
    {Ts, Obj} = parse_tokens(Ts0, []),
    parse_tokens(Ts, [{normalise(S), Obj}|Ps]);
parse_tokens([end_object|Ts], Ps) ->
    {Ts, lists:reverse(Ps)}.

normalise(S) ->
    case erl_parse:parse_exprs(S) of
        {ok, E} ->
            case E of
                ?Q("_@Key = _@Value") ->
                    {erl_parse:normalise(Key),
                     erl_parse:normalise(Value)};
                ?Q("_@Key") ->
                    erl_parse:normalise(Key)
            end;
        Err ->
            throw(Err)
    end.
