-module(upgradee).

-export([dispatch_loop/0]).

-ifdef(VERSION_1).
-define(VERSION,1).

-export([exp1/0]).
-export([exp1exp2/0]).
-export([exp1loc2/0]).

exp1() ->
    {?VERSION, loc1exp2()}.

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

dispatch_loop() ->
    Msg = receive M -> M end,
    erlang:display({"upgradee version", ?VERSION, "got msg", Msg}),
    {Func,Ret} = case Msg of
		     {Pid, local, F=exp1} ->
			 {F, local_exp1()};
		     {Pid, local, F=exp1exp2} ->
			 {F, catch exp1exp2()};
		     {Pid, local, F=exp1loc2} ->
			 {F, catch exp1loc2()};
		     {Pid, local, F=loc1exp2} ->
			 {F, catch loc1exp2()};
		     {Pid, local, F=exp2} ->
			 {F, local_exp2()};

                     {Pid, external, F=exp1} ->
			 {F, catch ?MODULE:exp1()};
		     {Pid, external, F=exp1exp2} ->
			 {F, catch ?MODULE:exp1exp2()};
		     {Pid, external, F=exp1loc2} ->
			 {F, catch ?MODULE:exp1loc2()};
		     {Pid, external, F=loc1exp2} ->
			 {F, catch ?MODULE:loc1exp2()};
		     {Pid, external, F=exp2} ->
			 {F, catch ?MODULE:exp2()}
		 end,
    Pid ! {self(), call_result, Func, Ret},

    dispatch_loop(). % A local call, we don't want to upgrade the dispatcher



-ifdef(VERSION_1).
local_exp1() -> catch exp1().
-else.
local_exp1() ->
    erlang:display({"upgradee:local_exp1 in version", ?VERSION}),
    {cannot_compile,?VERSION}.
-endif.

-ifdef(VERSION_2).
local_exp2() -> catch exp2().
-else.
local_exp2() -> 
    erlang:display({"upgradee:local_exp2 in version", ?VERSION}),
    {cannot_compile,?VERSION}.
-endif.
