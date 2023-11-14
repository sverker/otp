%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2023. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

-module(trace_session_SUITE).

-export([all/0, suite/0]).

-export([
         basic/1,
         end_of_list/1]).

-include_lib("common_test/include/ct.hrl").


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() ->
    [
     basic,
     end_of_list].


basic(_Config) ->
    Tester = self(),
    Tracer0 = spawn_link(fun() -> tracer(Tester) end),
    Tracer1 = spawn_link(fun() -> tracer(Tester) end),
    Tracer2 = spawn_link(fun() -> tracer(Tester) end),

    basic_do1(Tracer0, [{tracer, Tracer0}]),

    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    basic_do2(Tracer1, [{session,S1}],
              Tracer2, [{session,S2}]),
    basic_do2(Tracer0, [{tracer,Tracer0}],
              Tracer2, [{session,S2}]),
    basic_do2(Tracer2, [{session,S2}],
              Tracer0, [{tracer,Tracer0}]),

    basic_do2(Tracer1, [{session,S1}],
              Tracer0, [{tracer,Tracer0},{session,S0}]),
    basic_do2(Tracer0, [{tracer,Tracer0},{session,S0}],
              Tracer1, [{session,S1}]),

    ok = erlang:trace_session_destroy(S0),
    ok = erlang:trace_session_destroy(S1),
    ok = erlang:trace_session_destroy(S2),

    unlink(Tracer0),
    exit(Tracer0, die),
    unlink(Tracer1),
    exit(Tracer1, die),
    unlink(Tracer2),
    exit(Tracer2, die),

    ok.

basic_do1(Tracer, Opts) ->
    1 = erlang:trace(self(), true, [procs | Opts]),

    Tracee = self(),
    RegName = ?MODULE,
    register(RegName, Tracee),
    unregister(RegName),

    {Tracer, {trace, Tracee, register, RegName}} = receive_any(),
    {Tracer, {trace, Tracee, unregister, RegName}} = receive_any(),

    1 = erlang:trace(self(), false, [procs | Opts]),

    register(RegName, Tracee),
    unregister(RegName),

    timeout = receive_any(),

    ok.

basic_do2(Tracer1, Opts1, Tracer2, Opts2) ->
    1 = erlang:trace(self(), true, [procs | Opts1]),
    1 = erlang:trace(self(), true, [procs | Opts2]),

    Tracee = self(),
    RegName = ?MODULE,
    register(RegName, Tracee),
    unregister(RegName),

    {Ta, {trace, Tracee, register, RegName}} = receive_any(),
    {Tb, {trace, Tracee, register, RegName}} = receive_any(),
    true = (#{Ta => v, Tb => v} =:= #{Tracer1 => v, Tracer2 => v}),

    {Tc, {trace, Tracee, unregister, RegName}} = receive_any(),
    {Td, {trace, Tracee, unregister, RegName}} = receive_any(),
    true = (#{Tc => v, Td => v} =:= #{Tracer1 => v, Tracer2 => v}),

    1 = erlang:trace(self(), false, [procs | Opts1]),

    register(RegName, Tracee),
    unregister(RegName),

    {Tracer2, {trace, Tracee, register, RegName}} = receive_any(),
    {Tracer2, {trace, Tracee, unregister, RegName}} = receive_any(),

    1 = erlang:trace(self(), false, [procs | Opts2]),

    register(RegName, Tracee),
    unregister(RegName),

    timeout = receive_any(),

    ok.


tracer(Tester) ->
    receive M ->
            io:format("Tracer ~p got message: ~p\n", [self(), M]),
            Tester ! {self(), M}
    end,
    tracer(Tester).


receive_any() ->
    receive_any(100).

receive_any(Timeout) ->
    receive M -> M
    after Timeout -> timeout
    end.

end_of_list(_Config) ->
    ok.
