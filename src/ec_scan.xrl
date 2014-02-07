%% -*- mode: erlang -*-
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

Definitions.

WS = \s\t\r\n
OBJ = \{\}
KEYCHAR = [^\'=#{WS}{OBJ}]
QUOTED = \'(\\.|[^\'])*\'
KEY = ({KEYCHAR}+|{QUOTED})[{WS}]*

Rules.

#.*\n : skip_token.
[{WS}]+ : skip_token.
{KEY}=(\\.|[^\n])* : {token, prop(TokenChars, TokenLine)}.
{KEY}\{ : {token, object(TokenChars, TokenLine)}.
\} : {token, end_object}.

Erlang code.

prop(Chars, Line) ->
    {expr, scan(drop_trailing_ws(Chars), Line)}.

object(Chars, Line) ->
    {begin_object, scan(string:strip(Chars, right, ${), Line)}.

scan(Chars, Line) ->
    {ok, Tokens, _} = erl_scan:string(unescape(Chars) ++ ".", Line),
    Tokens.


unescape($n) -> $\n;
unescape($r) -> $\r;
unescape($t) -> $\t;
unescape($s) -> $\s;
unescape(C) when is_integer(C) -> C;
unescape(Cs) when is_list(Cs) -> unescape(Cs, []).

unescape([], Acc) ->
    lists:reverse(Acc);
unescape([$\\,C|Cs], Acc) when C < $0, C > $9 ->
    unescape(Cs, [unescape(C)|Acc]);
unescape([C|Cs], Acc) ->
    unescape(Cs, [C|Acc]).


drop_trailing_ws(Chars) ->
    lists:foldr(
      fun ($\s, []) -> [];
          ($\t, []) -> [];
          ($\n, []) -> [];
          ($=, []) ->[];
          (C, Acc) -> [C|Acc]
      end,
      [], Chars).
