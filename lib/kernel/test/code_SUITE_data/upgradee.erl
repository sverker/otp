-module(upgradee).

-ifdef(VERSION_1).
-define(VERSION,1).

-export([exp1/0]).
-export([exp1exp2/0]).
-export([exp1loc2/0]).

exp1() -> {?VERSION, loc1exp2()}.

-endif. % VERSION_1

-ifdef(VERSION_2).
-define(VERSION,2).

-export([exp2/0]).
-export([exp1exp2/0]).
-export([loc1exp2/0]).

exp2() -> {?VERSION, exp1loc2()}.

-endif. % VERSION_2


exp1exp2() -> ?VERSION.
exp1loc2() -> ?VERSION.
loc1exp2() -> ?VERSION.

