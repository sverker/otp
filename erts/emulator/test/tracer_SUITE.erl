%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2013. All Rights Reserved.
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

-module(tracer_SUITE).

%%%
%%% Tests the tracer module interface
%%%

-export([all/0, suite/0,groups/0, init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).
-export([enabled/3, trace/5, trace/6]).
-export([send/1, recv/1, spawn/1, exit/1, link/1, unlink/1,
         getting_linked/1, getting_unlinked/1, register/1, unregister/1,
         in/1, out/1, gc_start/1, gc_end/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() ->
    [{group, basic}].

groups() ->
    [{ basic, [], [send, recv, spawn, exit, link, unlink, getting_linked,
                   getting_unlinked, register, unregister, in, out,
                   gc_start, gc_end]}].

init_per_suite(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    case catch enabled(trace_status, self(), self()) of
        discard ->
            ok;
        _ ->
            ok = erlang:load_nif(filename:join(DataDir, atom_to_list(?MODULE)), 0)
    end,
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

send(_Config) ->

    Self = self(),
    Tc = fun(Pid) ->
                 Pid ! fun() -> Self ! ok end,
                 receive ok -> ok after 100 -> ct:fail(timeout) end
         end,

    Expect = fun(Pid, State) ->
                     Opts = #{ timestamp => undefined,
                               scheduler_id => undefined,
                               match_spec_result => true },
                     receive
                         Msg ->
                             {Pid, send, State, Pid, ok, Self, Opts} = Msg
                     end
             end,
    test(send, Tc, Expect).


recv(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! ok
         end,

    Expect = fun(Pid, State) ->
                     Opts = #{ timestamp => undefined,
                               scheduler_id => undefined,
                               match_spec_result => true },
                     receive
                         Msg ->
                             {undefined, 'receive', State, Pid, ok, undefined, Opts} = Msg
                     end
             end,

    test('receive', Tc, Expect, false).

spawn(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() -> erlang:spawn(lists,seq,[1,10]), ok end
         end,

    Expect =
        fun(Pid, State) ->
                Opts = #{ timestamp => undefined,
                          scheduler_id => undefined,
                          match_spec_result => true },
                receive
                    Msg ->
                        {Pid, spawn, State, Pid, NewPid,
                         {lists,seq,[1,10]}, Opts} = Msg,
                        true = is_pid(NewPid) andalso NewPid /= Pid
                end
             end,

    test(spawn, procs, Tc, Expect, true).

exit(_Config) ->
    Tc = fun(Pid) ->
                 Pid ! fun() -> exit end
         end,

    Expect =
        fun(Pid, State) ->
                Opts = #{ timestamp => undefined,
                          scheduler_id => undefined,
                          match_spec_result => true },
                receive
                    Msg ->
                        {Pid, exit, State, Pid, normal, undefined, Opts} = Msg
                end
             end,

    test(exit, procs, Tc, Expect, true, true).

link(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               SPid = erlang:spawn(fun() -> receive _ -> ok end end),
                               erlang:link(SPid),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State) ->
                Opts = #{ timestamp => undefined,
                          scheduler_id => undefined,
                          match_spec_result => true },
                receive
                    Msg ->
                        {Pid, link, State, Pid, NewPid, undefined, Opts} = Msg,
                        true = is_pid(NewPid) andalso NewPid /= Pid
                end
             end,

    test(link, procs, Tc, Expect, true).

unlink(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               SPid = erlang:spawn(fun() -> receive _ -> ok end end),
                               erlang:link(SPid),
                               erlang:unlink(SPid),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State) ->
                Opts = #{ timestamp => undefined,
                          scheduler_id => undefined,
                          match_spec_result => true },
                receive
                    Msg ->
                        {Pid, unlink, State, Pid, NewPid, undefined, Opts} = Msg,
                        true = is_pid(NewPid) andalso NewPid /= Pid
                end
             end,

    test(unlink, procs, Tc, Expect, true).

getting_linked(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               Self = self(),
                               erlang:spawn(fun() -> erlang:link(Self) end),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State) ->
                Opts = #{ timestamp => undefined,
                          scheduler_id => undefined,
                          match_spec_result => true },
                receive
                    Msg ->
                        {NewPid, getting_linked, State, Pid, NewPid, undefined, Opts} = Msg,
                        true = is_pid(NewPid) andalso NewPid /= Pid
                end
             end,

    test(getting_linked, procs, Tc, Expect, false).

getting_unlinked(_Config) ->
    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               Self = self(),
                               erlang:spawn(fun() ->
                                                    erlang:link(Self),
                                                    erlang:unlink(Self)
                                            end),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State) ->
                Opts = #{ timestamp => undefined,
                          scheduler_id => undefined,
                          match_spec_result => true },
                receive
                    Msg ->
                        {NewPid, getting_unlinked, State, Pid, NewPid, undefined, Opts} = Msg,
                        true = is_pid(NewPid) andalso NewPid /= Pid
                end
             end,

    test(getting_unlinked, procs, Tc, Expect, false).

register(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               erlang:register(?MODULE, self()),
                               erlang:unregister(?MODULE),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State) ->
                Opts = #{ timestamp => undefined,
                          scheduler_id => undefined,
                          match_spec_result => true },
                receive
                    Msg ->
                        {Pid, register, State, Pid, ?MODULE, undefined, Opts} = Msg
                end
             end,

    test(register, procs, Tc, Expect, true).

unregister(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               erlang:register(?MODULE, self()),
                               erlang:unregister(?MODULE),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State) ->
                Opts = #{ timestamp => undefined,
                          scheduler_id => undefined,
                          match_spec_result => true },
                receive
                    Msg ->
                        {Pid, unregister, State, Pid, ?MODULE, undefined, Opts} = Msg
                end
             end,

    test(unregister, procs, Tc, Expect, true).

in(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! ok
         end,

    Expect =
        fun(Pid, State) ->
                Opts = #{ timestamp => undefined,
                          scheduler_id => undefined,
                          match_spec_result => true },
                receive
                    {Pid, in, _, _, {erlang, apply, _}, _, _} ->
                        %% This may arrive here because of race
                        %% in process start and trace start.
                        %% Ignore it.
                        receive
                            Msg ->
                                {Pid, in, State, Pid, _, undefined, Opts} = Msg
                        end;
                    Msg ->
                        {Pid, in, State, Pid, _, undefined, Opts} = Msg
                end
             end,

    test(in, running, Tc, Expect, true).

out(_Config) ->
    Tc = fun(Pid) ->
                 Pid ! fun() -> receive after 10 -> exit end end,
                 Ref = erlang:monitor(process, Pid),
                 receive {'DOWN', Ref, _, _, _} -> ok end
         end,

    Expect =
        fun(Pid, State) ->
                Opts = #{ timestamp => undefined,
                          scheduler_id => undefined,
                          match_spec_result => true },

                %% We cannot predict how many out schedules there will be
                (fun F() ->
                         receive
                             Msg ->
                                 {Pid, out, State, Pid, _, undefined, Opts} = Msg,
                                 F()
                         after 0 -> ok
                         end
                 end)()
             end,

    test(out, running, Tc, Expect, true, true).

gc_start(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               erlang:garbage_collect(),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State) ->
                Opts = #{ timestamp => undefined,
                          scheduler_id => undefined,
                          match_spec_result => true },
                receive
                    Msg ->
                        {Pid, gc_start, State, Pid, _, undefined, Opts} = Msg
                end
             end,

    test(gc_start, garbage_collection, Tc, Expect, true).

gc_end(_Config) ->

    Tc = fun(Pid) ->
                 Pid ! fun() ->
                               erlang:garbage_collect(),
                               ok
                       end
         end,

    Expect =
        fun(Pid, State) ->
                Opts = #{ timestamp => undefined,
                          scheduler_id => undefined,
                          match_spec_result => true },
                receive
                    Msg ->
                        {Pid, gc_end, State, Pid, _, undefined, Opts} = Msg
                end
             end,

    test(gc_end, garbage_collection, Tc, Expect, true).

test(Event, Tc, Expect) ->
    test(Event, Tc, Expect, true).
test(Event, Tc, Expect, Removes) ->
    test(Event, Event, Tc, Expect, Removes).
test(Event, TraceFlag, Tc, Expect, Removes) ->
    test(Event, TraceFlag, Tc, Expect, Removes, false).
test(Event, TraceFlag, Tc, Expect, Removes, Dies) ->

    ComplexState = {fun() -> ok end, <<0:(128*8)>>},

    %% Test that trace works
    State1 = {#{ Event => trace }, self(), ComplexState},
    Pid1 = start_tracee(),
    1 = erlang:trace(Pid1, true, [TraceFlag, {tracer, ?MODULE, State1}]),
    Tc(Pid1),
    ok = trace_delivered(Pid1),

    Expect(Pid1, State1),
    receive M11 -> ct:fail({unexpected, M11}) after 0 -> ok end,
    if not Dies ->
            {flags, [TraceFlag]} = erlang:trace_info(Pid1, flags),
            {tracer, {?MODULE, State1}} = erlang:trace_info(Pid1, tracer),
            erlang:trace(Pid1, false, [TraceFlag]);
       true -> ok
    end,

    %% Test that  discard works
    Pid2 = start_tracee(),
    State2 = {#{ Event => discard }, self(), ComplexState},
    1 = erlang:trace(Pid2, true, [TraceFlag, {tracer, ?MODULE, State2}]),
    Tc(Pid2),
    ok = trace_delivered(Pid2),
    receive M2 -> ct:fail({unexpected, M2}) after 0 -> ok end,
    if not Dies ->
            {flags, [TraceFlag]} = erlang:trace_info(Pid2, flags),
            {tracer, {?MODULE, State2}} = erlang:trace_info(Pid2, tracer),
            erlang:trace(Pid2, false, [TraceFlag]);
       true ->
            ok
    end,

    %% Test that remove works
    Pid3 = start_tracee(),
    State3 = {#{ Event => remove }, self(), ComplexState},
    1 = erlang:trace(Pid3, true, [TraceFlag, {tracer, ?MODULE, State3}]),
    Tc(Pid3),
    ok = trace_delivered(Pid3),
    receive M3 -> ct:fail({unexpected, M3}) after 0 -> ok end,
    if not Dies ->
            if Removes ->
                    {flags, []} = erlang:trace_info(Pid3, flags),
                    {tracer, []} = erlang:trace_info(Pid3, tracer);
               true ->
                    {flags, [TraceFlag]} = erlang:trace_info(Pid3, flags),
                    {tracer, {?MODULE, State3}} = erlang:trace_info(Pid3, tracer)
            end,
            erlang:trace(Pid3, false, [TraceFlag]);
       true ->
            ok
    end,
    ok.

start_tracee() ->
    spawn_link(
      fun F() ->
              receive
                  Action when is_function(Action) ->
                      case Action() of
                          ok ->
                              F();
                          Err ->
                              Err
                      end;
                  _ ->
                      F()
              end
      end).

trace_delivered(Pid) ->
    Ref = erlang:trace_delivered(Pid),
    receive
        {trace_delivered, Pid, Ref} ->
            ok
    after 1000 ->
            timeout
    end.

%%%
%%% NIF placeholders
%%%

enabled(_, _, _) ->
    erlang:nif_error(nif_not_loaded).

trace(_, _, _, _, _) ->
    erlang:nif_error(nif_not_loaded).

trace(_, _, _, _, _, _) ->
    erlang:nif_error(nif_not_loaded).
