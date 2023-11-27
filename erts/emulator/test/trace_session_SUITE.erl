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

-export([test_set_on_spawn/1,
         test_set_on_first_spawn/1,
         test_set_on_link/1,
         test_set_on_first_link/1,
         procs/1,
         basic/1,
         call/1,
         end_of_list/1]).

-include_lib("common_test/include/ct.hrl").


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 1}}].

all() ->
    [
     basic,
     call,
     procs,
     test_set_on_spawn,
     test_set_on_first_spawn,
     test_set_on_link,
     test_set_on_first_link,
     end_of_list].

test_set_on_spawn(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),
    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    set_on_spawn(Tracer1, [{session, S1}]),
    set_on_spawn2(Tracer1, [{session,S1}],
            Tracer2, [{session,S2}]),
    set_on_spawn2(Tracer0, [{tracer,Tracer0}],
            Tracer2, [{session,S2}]),
    set_on_spawn2(Tracer2, [{session,S2}],
            Tracer0, [{tracer,Tracer0}]),

    set_on_spawn2(Tracer1, [{session,S1}],
            Tracer0, [{tracer,Tracer0},{session,S0}]),
    set_on_spawn2(Tracer0, [{tracer,Tracer0},{session,S0}],
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
test_set_on_first_spawn(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),
    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    set_on_first_spawn(Tracer1, [{session, S1}]),
    set_on_first_spawn2(Tracer1, [{session,S1}],
            Tracer2, [{session,S2}]),
    set_on_first_spawn2(Tracer0, [{tracer,Tracer0}],
            Tracer2, [{session,S2}]),
    set_on_first_spawn2(Tracer2, [{session,S2}],
            Tracer0, [{tracer,Tracer0}]),

    set_on_first_spawn2(Tracer1, [{session,S1}],
            Tracer0, [{tracer,Tracer0},{session,S0}]),
    set_on_first_spawn2(Tracer0, [{tracer,Tracer0},{session,S0}],
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
test_set_on_link(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),
    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    set_on_link(Tracer1, [{session, S1}]),
    set_on_link2(Tracer1, [{session,S1}],
            Tracer2, [{session,S2}]),
    set_on_link2(Tracer0, [{tracer,Tracer0}],
            Tracer2, [{session,S2}]),
    set_on_link2(Tracer2, [{session,S2}],
            Tracer0, [{tracer,Tracer0}]),

    set_on_link2(Tracer1, [{session,S1}],
            Tracer0, [{tracer,Tracer0},{session,S0}]),
    set_on_link2(Tracer0, [{tracer,Tracer0},{session,S0}],
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
test_set_on_first_link(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),
    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    set_on_first_link(Tracer1, [{session, S1}]),
    set_on_first_link2(Tracer1, [{session,S1}],
            Tracer2, [{session,S2}]),
    set_on_first_link2(Tracer0, [{tracer,Tracer0}],
            Tracer2, [{session,S2}]),
    set_on_first_link2(Tracer2, [{session,S2}],
            Tracer0, [{tracer,Tracer0}]),

    set_on_first_link2(Tracer1, [{session,S1}],
            Tracer0, [{tracer,Tracer0},{session,S0}]),
    set_on_first_link2(Tracer0, [{tracer,Tracer0},{session,S0}],
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
set_on_spawn(Tracer, Opts) ->
    1 = erlang:trace(self(), true, [procs, set_on_spawn | Opts]),
    Tracee = self(),
    TraceeChild = spawn(fun() -> Pid = spawn(fun() -> receive M -> M end end), exit(Pid, die), receive M -> M end end),
    timer:sleep(100),
    exit(TraceeChild, die),
    timer:sleep(100),
    {Tracer,{trace, Tracee, spawn,TraceeChild,_}} = receive_any(),
    {Tracer,{trace, TraceeChild, spawned,Tracee,_}} = receive_any(),
    {Tracer,{trace, TraceeChild, spawn,_TraceeChild2,_}} = receive_any(),
    {Tracer,{trace, _TraceeChild2, spawned,TraceeChild,_}} = receive_any(),
    {Tracer,{trace, _TraceeChild2, exit, die}} = receive_any(),
    {Tracer,{trace, TraceeChild, exit, die}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_spawn | Opts]),
    ok.
set_on_spawn2(Tracer1, Opts1, Tracer2, Opts2) ->
    1 = erlang:trace(self(), true, [procs, set_on_spawn | Opts1]),
    1 = erlang:trace(self(), true, [procs, set_on_spawn | Opts2]),
    Tracee = self(),
    TraceeChild = spawn(fun() -> Pid = spawn(fun() -> receive M -> M end end), exit(Pid, die), receive M -> M end  end),
    timer:sleep(100),
    exit(TraceeChild, die),
    {Ta,{trace, Tracee, spawn,TraceeChild,_}} = receive_any(),
    {Tb,{trace, Tracee, spawn,TraceeChild,_}} = receive_any(),
    true = (#{Ta => v, Tb => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tc,{trace, TraceeChild, spawned,Tracee,_}} = receive_any(),
    {Td,{trace, TraceeChild, spawned,Tracee,_}} = receive_any(),
    true = (#{Tc => v, Td => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Te,{trace, TraceeChild, spawn,_TraceeChild2,_}} = receive_any(),
    {Tf,{trace, TraceeChild, spawn,_TraceeChild2,_}} = receive_any(),
    true = (#{Te => v, Tf => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tg,{trace, _TraceeChild2, spawned,TraceeChild,_}} = receive_any(),
    {Th,{trace, _TraceeChild2, spawned,TraceeChild,_}} = receive_any(),
    true = (#{Tg => v, Th => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Ti,{trace, _TraceeChild2, exit, die}} = receive_any(),
    {Tj,{trace, _TraceeChild2, exit, die}} = receive_any(),
    true = (#{Ti => v, Tj => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tk,{trace, TraceeChild, exit, die}} = receive_any(),
    {Tl,{trace, TraceeChild, exit, die}} = receive_any(),
    true = (#{Tk => v, Tl => v} =:= #{Tracer1 => v, Tracer2 => v}),
    1 = erlang:trace(self(), false, [procs, set_on_spawn | Opts1]),
    TraceeChild1 = spawn(fun() -> Pid = spawn(fun() -> receive M -> M end end), exit(Pid, die), receive M -> M end  end),
    timer:sleep(100),
    exit(TraceeChild1, die),
    timer:sleep(100),
    {Tracer2,{trace, Tracee, spawn,TraceeChild1,_}} = receive_any(),
    {Tracer2,{trace, TraceeChild1, spawned,Tracee,_}} = receive_any(),
    {Tracer2,{trace, TraceeChild1, spawn,_TraceeChild21,_}} = receive_any(),
    {Tracer2,{trace, _TraceeChild21, spawned,TraceeChild1,_}} = receive_any(),
    {Tracer2,{trace, _TraceeChild21, exit, die}} = receive_any(),
    {Tracer2,{trace, TraceeChild1, exit, die}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_spawn | Opts2]),
    ok.
set_on_first_spawn(Tracer, Opts) ->
    1 = erlang:trace(self(), true, [procs, set_on_first_spawn | Opts]),
    Tracee = self(),
    TraceeChild = spawn(fun() -> Pid = spawn(fun() -> receive M -> M end end), exit(Pid, die), receive M -> M end  end),
    timer:sleep(100),
    exit(TraceeChild, die),
    TraceeChild1 = spawn(fun() -> receive M -> M end end),
    exit(TraceeChild1, die),
    timer:sleep(100),
    {Tracer,{trace, Tracee, spawn,TraceeChild,_}} = receive_any(),
    {Tracer,{trace, TraceeChild, spawned,Tracee,_}} = receive_any(),
    {Tracer,{trace, TraceeChild, spawn,_TraceeChild2,_}} = receive_any(),
    {Tracer,{trace, Tracee, spawn,TraceeChild1,_}} = receive_any(),
    {Tracer,{trace, TraceeChild, exit, die}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_first_spawn | Opts]),
    ok.
set_on_first_spawn2(Tracer1, Opts1, Tracer2, Opts2) ->
    1 = erlang:trace(self(), true, [procs, set_on_first_spawn | Opts1]),
    1 = erlang:trace(self(), true, [procs, set_on_first_spawn | Opts2]),
    Tracee = self(),
    TraceeChild = spawn(fun() -> Pid = spawn(fun() -> receive M -> M end end), exit(Pid, die), receive M -> M end  end),
    timer:sleep(100),
    exit(TraceeChild, die),
    TraceeChild1 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    exit(TraceeChild1, die),
    timer:sleep(100),
    {Ta,{trace, Tracee, spawn,TraceeChild,_}} = receive_any(),
    {Tb,{trace, Tracee, spawn,TraceeChild,_}} = receive_any(),
    true = (#{Ta => v, Tb => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tc,{trace, TraceeChild, spawned,Tracee,_}} = receive_any(),
    {Td,{trace, TraceeChild, spawned,Tracee,_}} = receive_any(),
    true = (#{Tc => v, Td => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Te,{trace, TraceeChild, spawn,_TraceeChild2,_}} = receive_any(),
    {Tf,{trace, TraceeChild, spawn,_TraceeChild2,_}} = receive_any(),
    true = (#{Te => v, Tf => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tg,{trace, Tracee, spawn,TraceeChild1,_}} = receive_any(),
    {Th,{trace, Tracee, spawn,TraceeChild1,_}} = receive_any(),
    true = (#{Tg => v, Th => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Ti,{trace, TraceeChild, exit, die}} = receive_any(),
    {Tj,{trace, TraceeChild, exit, die}} = receive_any(),
    true = (#{Ti => v, Tj => v} =:= #{Tracer1 => v, Tracer2 => v}),
    1 = erlang:trace(self(), false, [procs, set_on_first_spawn | Opts1]),
    TraceeChild2 = spawn(fun() -> Pid = spawn(fun() -> receive M -> M end end), exit(Pid, die), receive M -> M end  end),
    timer:sleep(100),
    exit(TraceeChild2, die),
    timer:sleep(100),
    TraceeChild21 = spawn(fun() -> receive M -> M end end),
    exit(TraceeChild21, die),
    timer:sleep(100),

    {Tracer2,{trace, Tracee, spawn,TraceeChild2,_}} = receive_any(),
    {Tracer2,{trace, Tracee, spawn,TraceeChild21,_}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_first_spawn | Opts2]),
    ok.
set_on_link(Tracer, Opts) ->
    1 = erlang:trace(self(), true, [procs, set_on_link | Opts]),
    Tracee = self(),
    TraceeChild = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    unlink(TraceeChild),
    timer:sleep(100),
    exit(TraceeChild, die),
    timer:sleep(100),
    {Tracer,{trace, Tracee, spawn,TraceeChild,_}} = receive_any(),
    {Tracer,{trace, Tracee, link, TraceeChild}} = receive_any(),
    {Tracer,{trace, TraceeChild, spawned,Tracee,_}} = receive_any(),
    {Tracer,{trace, TraceeChild, getting_linked, Tracee}} = receive_any(),
    {Tracer,{trace, Tracee, unlink, TraceeChild}} = receive_any(),
    {Tracer,{trace, TraceeChild, getting_unlinked, Tracee}} = receive_any(),
    {Tracer,{trace, TraceeChild, exit, die}} = receive_any(),

    %%TODO Test link/1, did it work before multiple trace sessions?
    %%TraceeChild2 = spawn(fun() -> receive M -> M end end),
    %%timer:sleep(100),
    %%link(TraceeChild2),
    %%timer:sleep(100),
    %%unlink(TraceeChild2),
    %%timer:sleep(100),
    %%exit(TraceeChild2, die),
    %%timer:sleep(100),
    %%{Tracer,{trace, Tracee, spawn,TraceeChild2,_}} = receive_any(),
    %%{Tracer,{trace, Tracee, link, TraceeChild2}} = receive_any(),
    %%{Tracer,{trace, TraceeChild2, getting_linked, Tracee}} = receive_any(),
    %%{Tracer,{trace, Tracee, unlink, TraceeChild2}} = receive_any(),
    %%{Tracer,{trace, TraceeChild2, getting_unlinked, Tracee}} = receive_any(),
    %%{Tracer,{trace, TraceeChild2, exit, die}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_link | Opts]),
    ok.
set_on_link2(Tracer1, Opts1, Tracer2, Opts2) ->
    1 = erlang:trace(self(), true, [procs, set_on_link | Opts1]),
    1 = erlang:trace(self(), true, [procs, set_on_link | Opts2]),
    Tracee = self(),
    TraceeChild = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    unlink(TraceeChild),
    timer:sleep(100),
    exit(TraceeChild, die),
    timer:sleep(100),
    {Ta,{trace, Tracee, spawn,TraceeChild,_}} = receive_any(),
    {Tb,{trace, Tracee, spawn,TraceeChild,_}} = receive_any(),
    true = (#{Ta => v, Tb => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tc,{trace, Tracee, link, TraceeChild}} = receive_any(),
    {Td,{trace, Tracee, link, TraceeChild}} = receive_any(),
    true = (#{Tc => v, Td => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Te,{trace, TraceeChild, spawned,Tracee,_}} = receive_any(),
    {Tf,{trace, TraceeChild, spawned,Tracee,_}} = receive_any(),
    true = (#{Te => v, Tf => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tg,{trace, TraceeChild, getting_linked, Tracee}} = receive_any(),
    {Th,{trace, TraceeChild, getting_linked, Tracee}} = receive_any(),
    true = (#{Tg => v, Th => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Ti,{trace, Tracee, unlink, TraceeChild}} = receive_any(),
    {Tj,{trace, Tracee, unlink, TraceeChild}} = receive_any(),
    true = (#{Ti => v, Tj => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tk,{trace, TraceeChild, getting_unlinked, Tracee}} = receive_any(),
    {Tl,{trace, TraceeChild, getting_unlinked, Tracee}} = receive_any(),
    true = (#{Tk => v, Tl => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tm,{trace, TraceeChild, exit, die}} = receive_any(),
    {Tn,{trace, TraceeChild, exit, die}} = receive_any(),
    true = (#{Tm => v, Tn => v} =:= #{Tracer1 => v, Tracer2 => v}),
    1 = erlang:trace(self(), false, [procs, set_on_link | Opts1]),
    TraceeChild1 = spawn_link(fun() -> receive M -> M end end),
    unlink(TraceeChild1),
    exit(TraceeChild1, die),
    timer:sleep(100),
    {Tracer2,{trace, Tracee, spawn,TraceeChild1,_}} = receive_any(),
    {Tracer2,{trace, Tracee, link, TraceeChild1}} = receive_any(),
    {Tracer2,{trace, TraceeChild1, spawned,Tracee,_}} = receive_any(),
    {Tracer2,{trace, TraceeChild1, getting_linked, Tracee}} = receive_any(),
    {Tracer2,{trace, Tracee, unlink, TraceeChild1}} = receive_any(),
    {Tracer2,{trace, TraceeChild1, getting_unlinked, Tracee}} = receive_any(),
    {Tracer2,{trace, TraceeChild1, exit, die}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_link | Opts2]),
    ok.
set_on_first_link(Tracer, Opts) ->
    1 = erlang:trace(self(), true, [procs, set_on_first_link | Opts]),
    Tracee = self(),
    TraceeChild = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    TraceeChild1 = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    unlink(TraceeChild),
    timer:sleep(100),
    exit(TraceeChild, die),
    timer:sleep(100),
    unlink(TraceeChild1),
    timer:sleep(100),
    exit(TraceeChild1, die),
    timer:sleep(100),
    {Tracer,{trace, Tracee, spawn,TraceeChild,_}} = receive_any(),
    {Tracer,{trace, Tracee, link, TraceeChild}} = receive_any(),
    {Tracer,{trace, TraceeChild, spawned,Tracee,_}} = receive_any(),
    {Tracer,{trace, TraceeChild, getting_linked, Tracee}} = receive_any(),
    {Tracer,{trace, Tracee, spawn,TraceeChild1,_}} = receive_any(),
    {Tracer,{trace, Tracee, link, TraceeChild1}} = receive_any(),
    {Tracer,{trace, Tracee, unlink, TraceeChild}} = receive_any(),
    {Tracer,{trace, TraceeChild, getting_unlinked, Tracee}} = receive_any(),
    {Tracer,{trace, TraceeChild, exit, die}} = receive_any(),
    {Tracer,{trace, Tracee, unlink, TraceeChild1}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_first_link | Opts]),
    ok.

set_on_first_link2(Tracer1, Opts1, Tracer2, Opts2) ->
    1 = erlang:trace(self(), true, [procs, set_on_first_link | Opts1]),
    1 = erlang:trace(self(), true, [procs, set_on_first_link | Opts2]),
    Tracee = self(),
    TraceeChild = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    TraceeChild1 = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    unlink(TraceeChild),
    timer:sleep(100),
    exit(TraceeChild, die),
    timer:sleep(100),
    unlink(TraceeChild1),
    timer:sleep(100),
    exit(TraceeChild1, die),
    timer:sleep(100),
    {Ta,{trace, Tracee, spawn,TraceeChild,_}} = receive_any(),
    {Tb,{trace, Tracee, spawn,TraceeChild,_}} = receive_any(),
    true = (#{Ta => v, Tb => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tc,{trace, Tracee, link, TraceeChild}} = receive_any(),
    {Td,{trace, Tracee, link, TraceeChild}} = receive_any(),
    true = (#{Tc => v, Td => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Te,{trace, TraceeChild, spawned,Tracee,_}} = receive_any(),
    {Tf,{trace, TraceeChild, spawned,Tracee,_}} = receive_any(),
    true = (#{Te => v, Tf => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tg,{trace, TraceeChild, getting_linked, Tracee}} = receive_any(),
    {Th,{trace, TraceeChild, getting_linked, Tracee}} = receive_any(),
    true = (#{Tg => v, Th => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Ti,{trace, Tracee, spawn,TraceeChild1,_}} = receive_any(),
    {Tj,{trace, Tracee, spawn,TraceeChild1,_}} = receive_any(),
    true = (#{Ti => v, Tj => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tk,{trace, Tracee, link, TraceeChild1}} = receive_any(),
    {Tl,{trace, Tracee, link, TraceeChild1}} = receive_any(),
    true = (#{Tk => v, Tl => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tm,{trace, Tracee, unlink, TraceeChild}} = receive_any(),
    {Tn,{trace, Tracee, unlink, TraceeChild}} = receive_any(),
    true = (#{Tm => v, Tn => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {To,{trace, TraceeChild, getting_unlinked, Tracee}} = receive_any(),
    {Tp,{trace, TraceeChild, getting_unlinked, Tracee}} = receive_any(),
    true = (#{To => v, Tp => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tq,{trace, TraceeChild, exit, die}} = receive_any(),
    {Tr,{trace, TraceeChild, exit, die}} = receive_any(),
    true = (#{Tq => v, Tr => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Ts,{trace, Tracee, unlink, TraceeChild1}} = receive_any(),
    {Tt,{trace, Tracee, unlink, TraceeChild1}} = receive_any(),
    true = (#{Ts => v, Tt => v} =:= #{Tracer1 => v, Tracer2 => v}),
    1 = erlang:trace(self(), false, [procs, set_on_first_link | Opts1]),
    TraceeChild2 = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    TraceeChild21 = spawn_link(fun() -> receive M -> M end end),
    timer:sleep(100),
    unlink(TraceeChild2),
    timer:sleep(100),
    exit(TraceeChild2, die),
    timer:sleep(100),
    unlink(TraceeChild21),
    timer:sleep(100),
    exit(TraceeChild21, die),
    timer:sleep(100),
    {Tracer2,{trace, Tracee, spawn,TraceeChild2,_}} = receive_any(),
    {Tracer2,{trace, Tracee, link, TraceeChild2}} = receive_any(),
    {Tracer2,{trace, Tracee, spawn,TraceeChild21,_}} = receive_any(),
    {Tracer2,{trace, Tracee, link, TraceeChild21}} = receive_any(),
    {Tracer2,{trace, Tracee, unlink, TraceeChild2}} = receive_any(),
    {Tracer2,{trace, Tracee, unlink, TraceeChild21}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_first_link | Opts2]),
    ok.
procs(_Config) ->
    Tester= self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0", Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1", Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2", Tester) end),

    procs_do1(Tracer0, [{tracer, Tracer0}]),

    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    procs_do2(Tracer1, [{session,S1}],
              Tracer2, [{session,S2}]),
    procs_do2(Tracer0, [{tracer,Tracer0}],
              Tracer2, [{session,S2}]),
    procs_do2(Tracer2, [{session,S2}],
              Tracer0, [{tracer,Tracer0}]),

    procs_do2(Tracer1, [{session,S1}],
              Tracer0, [{tracer,Tracer0},{session,S0}]),
    procs_do2(Tracer0, [{tracer,Tracer0},{session,S0}],
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

procs_do1(Tracer, Opts) ->
    1 = erlang:trace(self(), true, [procs, set_on_spawn | Opts]),

    Tracee = self(),
    RegName = ?MODULE,
    register(RegName, Tracee),
    timer:sleep(100),
    unregister(RegName),
    timer:sleep(100),
    TraceeChild = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild),
    timer:sleep(100),
    unlink(TraceeChild),
    timer:sleep(100),
    exit(TraceeChild, die),
    {Tracer, {trace, Tracee, register, RegName}} = receive_any(),
    {Tracer, {trace, Tracee, unregister, RegName}} = receive_any(),
    {Tracer, {trace, Tracee, spawn, TraceeChild, _}} = receive_any(),
    {Tracer, {trace, TraceeChild, spawned, Tracee, _}} = receive_any(),
    {Tracer, {trace, Tracee, link, TraceeChild}} = receive_any(),
    {Tracer, {trace, TraceeChild, getting_linked, Tracee}} = receive_any(),
    {Tracer, {trace, Tracee, unlink, TraceeChild}} = receive_any(),
    {Tracer, {trace, TraceeChild, getting_unlinked, Tracee}} = receive_any(),
    {Tracer, {trace, TraceeChild, exit, _Reason}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_spawn | Opts]),

    register(RegName, Tracee),
    timer:sleep(100),
    unregister(RegName),
    timer:sleep(100),
    TraceeChild1 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild1),
    timer:sleep(100),
    unlink(TraceeChild1),
    timer:sleep(100),
    exit(TraceeChild1, die),

    timeout = receive_any(),

    ok.
procs_do2(Tracer1, Opts1, Tracer2, Opts2) ->
    1 = erlang:trace(self(), true, [procs, set_on_spawn | Opts1]),
    1 = erlang:trace(self(), true, [procs, set_on_spawn | Opts2]),

    Tracee = self(),
    RegName = ?MODULE,
    register(RegName, Tracee),
    timer:sleep(100),
    unregister(RegName),
    timer:sleep(100),
    TraceeChild = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild),
    timer:sleep(100),
    unlink(TraceeChild),
    timer:sleep(100),
    exit(TraceeChild, die),
    {Ta, {trace, Tracee, register, RegName}} = receive_any(),
    {Tb, {trace, Tracee, register, RegName}} = receive_any(),
    true = (#{Ta => v, Tb => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tc, {trace, Tracee, unregister, RegName}} = receive_any(),
    {Td, {trace, Tracee, unregister, RegName}} = receive_any(),
    true = (#{Tc => v, Td => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Te, {trace, Tracee, spawn, TraceeChild, _}} = receive_any(),
    {Tf, {trace, Tracee, spawn, TraceeChild, _}} = receive_any(),
    true = (#{Te => v, Tf => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tg, {trace, TraceeChild, spawned, Tracee, _}} = receive_any(),
    {Th, {trace, TraceeChild, spawned, Tracee, _}} = receive_any(),
    true = (#{Tg => v, Th => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Ti, {trace, Tracee, link, TraceeChild}} = receive_any(),
    {Tj, {trace, Tracee, link, TraceeChild}} = receive_any(),
    true = (#{Ti => v, Tj => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tk, {trace, TraceeChild, getting_linked, Tracee}} = receive_any(),
    {Tl, {trace, TraceeChild, getting_linked, Tracee}} = receive_any(),
    true = (#{Tk => v, Tl => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tm, {trace, Tracee, unlink, TraceeChild}} = receive_any(),
    {Tn, {trace, Tracee, unlink, TraceeChild}} = receive_any(),
    true = (#{Tm => v, Tn => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {To, {trace, TraceeChild, getting_unlinked, Tracee}} = receive_any(),
    {Tp, {trace, TraceeChild, getting_unlinked, Tracee}} = receive_any(),
    true = (#{To => v, Tp => v} =:= #{Tracer1 => v, Tracer2 => v}),
    {Tq, {trace, TraceeChild, exit, _}} = receive_any(),
    {Tr, {trace, TraceeChild, exit, _}} = receive_any(),
    true = (#{Tr => v, Tq => v} =:= #{Tracer1 => v, Tracer2 => v}),
    1 = erlang:trace(self(), false, [procs, set_on_spawn | Opts1]),
    register(RegName, Tracee),
    timer:sleep(100),
    unregister(RegName),
    timer:sleep(100),
    TraceeChild1 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild1),
    timer:sleep(100),
    unlink(TraceeChild1),
    timer:sleep(100),
    exit(TraceeChild1, die),
    {Tracer2, {trace, Tracee, register, RegName}} = receive_any(),
    {Tracer2, {trace, Tracee, unregister, RegName}} = receive_any(),
    {Tracer2, {trace, Tracee, spawn, TraceeChild1, _}} = receive_any(),
    {Tracer2, {trace, TraceeChild1, spawned, Tracee, _}} = receive_any(),
    {Tracer2, {trace, Tracee, link, TraceeChild1}} = receive_any(),
    {Tracer2, {trace, TraceeChild1, getting_linked, Tracee}} = receive_any(),
    {Tracer2, {trace, Tracee, unlink, TraceeChild1}} = receive_any(),
    {Tracer2, {trace, TraceeChild1, getting_unlinked, Tracee}} = receive_any(),
    {Tracer2, {trace, TraceeChild1, exit, _}} = receive_any(),
    1 = erlang:trace(self(), false, [procs, set_on_spawn | Opts2]),
    register(RegName, Tracee),
    timer:sleep(100),
    unregister(RegName),
    timer:sleep(100),
    TraceeChild2 = spawn(fun() -> receive M -> M end end),
    timer:sleep(100),
    link(TraceeChild2),
    timer:sleep(100),
    unlink(TraceeChild2),
    timer:sleep(100),
    exit(TraceeChild2, die),
    timeout = receive_any(),
    ok.

basic(_Config) ->
    Tester = self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0",Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1",Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2",Tester) end),

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


call(_Config) ->
    Tester = self(),
    Tracer0 = spawn_link(fun() -> tracer("Tracer0",Tester) end),
    Tracer1 = spawn_link(fun() -> tracer("Tracer1",Tester) end),
    Tracer2 = spawn_link(fun() -> tracer("Tracer2",Tester) end),

    call_do1(Tracer0, [{tracer, Tracer0}]),

    S0 = erlang:trace_session_create([]),
    S1 = erlang:trace_session_create([{tracer,Tracer1}]),
    S2 = erlang:trace_session_create([{tracer,Tracer2}]),

    call_do2(Tracer1, [{session,S1}],
              Tracer2, [{session,S2}]),
    call_do2(Tracer0, [{tracer,Tracer0}],
              Tracer2, [{session,S2}]),
    call_do2(Tracer2, [{session,S2}],
              Tracer0, [{tracer,Tracer0}]),

    call_do2(Tracer1, [{session,S1}],
              Tracer0, [{tracer,Tracer0},{session,S0}]),
    call_do2(Tracer0, [{tracer,Tracer0},{session,S0}],
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

call_do1(Tracer, Opts) ->
    MFArity = {?MODULE,foo,0},
    MFArgs = {?MODULE,foo,[]},
    Tracee = self(),
    1 = erlang:trace(Tracee, true, [call | Opts]),
    1 = erlang:trace_pattern(MFArity, true, add_session([local], Opts)),

    foo(),

    {Tracer, {trace, Tracee, call, MFArgs}} = receive_any(),

    1 = erlang:trace(self(), false, [call | Opts]),

    foo(),
    timeout = receive_any(),

    ok.

call_do2(Tracer1, Opts1, Tracer2, Opts2) ->
    MFArity = {?MODULE,foo,0},
    MFArgs = {?MODULE,foo,[]},
    Tracee = self(),
    1 = erlang:trace(Tracee, true, [call | Opts1]),
    1 = erlang:trace(Tracee, true, [call | Opts2]),

    io:format("line ~p\n", [?LINE]),
    1 = erlang:trace_pattern(MFArity, true, add_session([local], Opts1)),
    foo(),
    {Tracer1, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),

    io:format("line ~p\n", [?LINE]),
    1 = erlang:trace_pattern(MFArity, true, add_session([local], Opts2)),
    foo(),
    {Ta, {trace, Tracee, call, MFArgs}} = receive_any(),
    {Tb, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),
    #{Tracer1 := v, Tracer2 := v} = #{Ta => v, Tb => v},

    io:format("line ~p\n", [?LINE]),
    1 = erlang:trace_pattern(MFArity, false, add_session([local], Opts2)),
    foo(),
    {Tracer1, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),

    io:format("line ~p\n", [?LINE]),
    1 = erlang:trace_pattern(MFArity, true, add_session([local], Opts2)),
    foo(),
    {Tc, {trace, Tracee, call, MFArgs}} = receive_any(),
    {Td, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),
    #{Tracer1 := v, Tracer2 := v} = #{Tc => v, Td => v},

    io:format("line ~p\n", [?LINE]),
    1 = erlang:trace_pattern(MFArity, false, add_session([local], Opts1)),
    foo(),
    {Tracer2, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),

    io:format("line ~p\n", [?LINE]),
    1 = erlang:trace_pattern(MFArity, true, add_session([local], Opts1)),
    foo(),
    {Te, {trace, Tracee, call, MFArgs}} = receive_any(),
    {Tf, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),
    #{Tracer1 := v, Tracer2 := v} = #{Te => v, Tf => v},

    io:format("line ~p\n", [?LINE]),
    1 = erlang:trace(self(), false, [call | Opts1]),
    foo(),
    {Tracer2, {trace, Tracee, call, MFArgs}} = receive_any(),
    timeout = receive_any(),

    io:format("line ~p\n", [?LINE]),
    1 = erlang:trace_pattern(MFArity, false, add_session([local], Opts2)),
    foo(),
    timeout = receive_any(),

    io:format("line ~p\n", [?LINE]),
    1 = erlang:trace_pattern(MFArity, false, add_session([local], Opts1)),
    1 = erlang:trace(self(), false, [call | Opts2]),
    foo(),
    timeout = receive_any(),

    ok.

foo() ->
    ok.

add_session(DstOpts, SrcOpts) ->
    case lists:keyfind(session, 1, SrcOpts) of
        {session, _}=S ->
            [S | DstOpts];
        false ->
            DstOpts
    end.

tracer(Name, Tester) ->
    receive M ->
            io:format("~p ~p got message: ~p\n", [Name, self(), M]),
            Tester ! {self(), M}
    end,
    tracer(Name, Tester).


receive_any() ->
    receive_any(100).

receive_any(Timeout) ->
    receive M -> M
    after Timeout -> timeout
    end.

end_of_list(_Config) ->
    ok.
