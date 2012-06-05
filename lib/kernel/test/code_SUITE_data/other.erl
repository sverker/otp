-module(other).

-ifdef(VERSION_1).
-define(VERSION,1).
-endif. % VERSION_1

-ifdef(VERSION_2).
-define(VERSION,2).
-endif. % VERSION_2

-export([exp1exp2/0]).

exp1exp2() -> ?VERSION.

