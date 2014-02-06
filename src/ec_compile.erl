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

-module(ec_compile).

-export([tokens/1]).

-include_lib("merl/include/merl.hrl").

-spec tokens(list()) -> {ok, list()} | error.
tokens(Ts) ->
    {ok, [compile_token(T) || T <- Ts]}.

compile_token({expr, S}) ->
    {ok, E} = erl_parse:parse_exprs(S),
    case E of
        ?Q("_@Key = _@Value") ->
            {erl_parse:normalise(Key),
             erl_parse:normalise(Value)}
    end.
