%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2001-2012. All Rights Reserved.
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
%%
-module(testTimer).
-export([go/0, tracer/0]).

-include_lib("test_server/include/test_server.hrl").

-define(times, 1250).
%-define(times, (1250 div 4)).
%-define(times, 1).

-define(par, 10).
%-define(par, erlang:system_info(schedulers_online)).

val() ->
    {'H323-UserInformation',{'H323-UU-PDU',
			     {callProceeding,
			      {'CallProceeding-UUIE',
			       {0,8,222},
			       {'EndpointType',
				{'NonStandardParameter',
				 {object,{0,9,237}},
				 <<"O">>},
				{'VendorIdentifier',
				 {'H221NonStandard',62,63,16282},
				 <<"OC">>,
				 <<"OC">>},
				{'GatekeeperInfo',
				 {'NonStandardParameter',
				  {object,{0,10,260}},
				  <<"O">>}},
				{'GatewayInfo',
				 [{h320,
				   {'H320Caps',
				    {'NonStandardParameter',
				     {object,{0,11,282}},
				     <<"O">>},
				    [{'DataRate',
				      {'NonStandardParameter',
				       {object,
					{0,11,295}},
				       <<"O">>},
				      1290470518,
				      78}],
				    [{'SupportedPrefix',
				      {'NonStandardParameter',
				       {object,
					{0,12,312}},
				       <<"O">>},
				      {'h323-ID',"BM"}}]}}],
				 {'NonStandardParameter',
				  {object,{0,13,326}},
				  <<"O">>}},
				{'McuInfo',
				 {'NonStandardParameter',
				  {object,{1,13,340,340}},
				  <<"OC">>}},
				{'TerminalInfo',
				 {'NonStandardParameter',
				  {object,{1,14,353,354}},
				  <<"OC">>}},
				true,
				true},
			       {ipxAddress,
				{'TransportAddress_ipxAddress',
				 <<"OCTET ">>,
				 <<"OCTE">>,
				 <<"OC">>}},
			       {'CallIdentifier',<<"OCTET STRINGOCTE">>},
			       {noSecurity,'NULL'},
			       [{'ClearToken',
				 1667517741,
				 "BM",
				 {'DHset',<<1:1>>,<<1:1>>,<<1:1>>},
				 <<"OCTET STR">>,
				 -26430296,
				 {'TypedCertificate',
				  {1,16,405,406},
				  <<"OC">>},
				 "BMP",
				 {'NonStandardParameter',
				  {1,16,414,415},
				  <<"OC">>}},
				{'ClearToken',
				 1817656756,
				 "BMP",
				 {'DHset',<<1:1>>,<<1:1>>,<<1:1>>},
				 <<"OCTET STRI">>,
				 -16356110,
				 {'TypedCertificate',
				  {1,17,442,443},
				  <<"OC">>},
				 "BMP",
				 {'NonStandardParameter',
				  {1,18,452,452},
				  <<"OC">>}}],
			       [{cryptoGKPwdEncr,
				 {'CryptoH323Token_cryptoGKPwdEncr',
				  {1,18,467,467},
				  {'Params',-7477016,<<"OCTET ST">>},
				  <<"OC">>}},
				{cryptoGKPwdEncr,
				 {'CryptoH323Token_cryptoGKPwdEncr',
				  {1,19,486,486},
				  {'Params',-2404513,<<"OCTET ST">>},
				  <<>>}}],
			       []}},
			     {'NonStandardParameter',{object,{0,3,84}},<<>>},
			     [],
			     true,
			     [],
			     []},
     {'H323-UserInformation_user-data',24,<<"O">>}}.
    

go() ->
    Module = 'H323-MESSAGES',
    Type = 'H323-UserInformation',
    Value = val(),
    Bytes = Module:encode(Type, Value),
    Value = Module:decode(Type, Bytes),

    catch proctracer:module_info(),

    BMPayload = {Module, Type, Value, Bytes},

    done = decode(2, Module, Type, Bytes),

    {ValWr,TraceWr} = tc(fun() -> encode(?times, Module, Type, Value) end),
    {ValRead,TraceRead} = tc(fun() -> decode(?times, Module, Type, Bytes) end),
    ct:pal("Baseline:~n"
           "ASN.1 encoding: ~.2f micro~n"
           "ASN.1 decoding: ~.2f micro~n", [ValWr / ?times / ?par,
                                            ValRead /?times / ?par]),

    Baseline = {ValWr, ValRead, TraceWr, TraceRead},

    erlang:trace_pattern({Module, '_', '_'}, [], [global]),
    erlang:trace_pattern({Module, '_', '_'}, [], [local]),

    run_benchmark("trace_pattern(~p, [global,local]):~n", BMPayload, Baseline),

    %% os:cmd("lttng destroy"),
    %% erlang:trace(self(), true, [call, {tracer, lttngtrace, []}]),
    %% {CallTraceValWr,done} = tc(fun() -> encode(?times, Module, Type, Value) end),
    %% {CallTraceValRead,done} = tc(fun() -> decode(?times, Module, Type, Bytes) end),
    %% ct:pal("trace_pattern(~p, [global,local]),~n"
    %%        "trace([call, {tracer, lttngtrace, []}]):~n"
    %%        "ASN.1 encoding: ~p micro (~.1f%)~n"
    %%        "ASN.1 decoding: ~p micro (~.1f%)~n",
    %%        [Module,
    %%         CallTraceValWr / ?times, CallTraceValWr / ValWr * 100.0 - 100,
    %%         CallTraceValRead /?times, CallTraceValRead / ValRead * 100.0 - 100]),

    %% os:cmd("lttng create"),
    %% os:cmd("lttng enable-event --userspace com_ericsson_otp:call"),
    %% os:cmd("lttng start"),
    %% {LttngCallTraceValWr,done} = tc(fun() -> encode(?times, Module, Type, Value) end),
    %% {LttngCallTraceValRead,done} = tc(fun() -> decode(?times, Module, Type, Bytes) end),
    %% ct:pal("trace_pattern(~p, [global,local]),~n"
    %%        "trace([call, {tracer, lttngtrace, []}]),~n"
    %%        "lttng create + start:~n"
    %%        "ASN.1 encoding: ~p micro (~.1f%)~n"
    %%        "ASN.1 decoding: ~p micro (~.1f%)~n",
    %%        [Module,
    %%         LttngCallTraceValWr / ?times, LttngCallTraceValWr / ValWr * 100.0 - 100,
    %%         LttngCallTraceValRead /?times, LttngCallTraceValRead / ValRead * 100.0 - 100]),
    %% os:cmd("lttng stop"),
    %% os:cmd("lttng destroy"),

    %% erlang:trace(self(), true, [call, arity, {tracer, lttngtrace, []}]),
    %% os:cmd("lttng create"),
    %% os:cmd("lttng enable-event --userspace com_ericsson_otp:call"),
    %% os:cmd("lttng start"),
    %% {LttngCallArityTraceValWr,done} = tc(fun() -> encode(?times, Module, Type, Value) end),
    %% {LttngCallArityTraceValRead,done} = tc(fun() -> decode(?times, Module, Type, Bytes) end),
    %% ct:pal("trace_pattern(~p, [global,local]),~n"
    %%        "trace([call, arity, {tracer, lttngtrace, []}]),~n"
    %%        "lttng create + start:~n"
    %%        "ASN.1 encoding: ~p micro (~.1f%)~n"
    %%        "ASN.1 decoding: ~p micro (~.1f%)~n",
    %%        [Module,
    %%         LttngCallArityTraceValWr / ?times, LttngCallArityTraceValWr / ValWr * 100.0 - 100,
    %%         LttngCallArityTraceValRead /?times, LttngCallArityTraceValRead / ValRead * 100.0 - 100]),
    %% os:cmd("lttng stop"),
    %% os:cmd("lttng destroy"),

%    erlang:process_flag(scheduler, 2),

    %% erlang:trace(self(), false, [call, arity]),
%    dbg:tracer(port, dbg:trace_port(file, "/dev/null")),
    dbg:tracer(port, fun() -> tracer() end),
    dbg:p(self(), [sos,call]),
    run_benchmark("trace_pattern(~p, [global,local]),~n"
                  "trace([call, {tracer, \"/dev/null\"}]),~n",
                  BMPayload, Baseline),

    dbg:p(self(), [sos,call, arity]),
    run_benchmark("trace_pattern(~p, [global,local]),~n"
                  "trace([call, arity, {tracer, \"/dev/null\"}]),~n",
                  BMPayload, Baseline),

    Comment = "encode: "++integer_to_list(round(ValWr/?times)) ++
	" micro, decode: "++integer_to_list(round(ValRead /?times)) ++
	" micro. [" ++ atom_to_list(Module:encoding_rule()) ++ "]",
    {comment,Comment}.

encode(0, _Module,_Type,_Value) ->
    done;
encode(N, Module,Type,Value) ->
    Module:encode(Type, Value),
    encode(N-1, Module, Type, Value).

decode(0, _Module, _Type, _Value) ->
    done;
decode(N, Module, Type, Value) ->
    Module:decode(Type, Value),
    decode(N-1, Module, Type, Value).


tc(Fun) ->
    Before = erlang:monotonic_time(1000000),
    par(Fun,?par),
    After = erlang:monotonic_time(1000000),
    Ref = erlang:trace_delivered(self()),
    receive {trace_delivered, _, Ref} -> done end,
    erlang:display({sync,sync_tracer()}),
    AfterTrace = erlang:monotonic_time(1000000),
    {After - Before, AfterTrace - Before}.

run_benchmark(Slogan, {Module, Type, Value, Bytes}, {BaseEnc, BaseDec, BaseEncT, BaseDecT}) ->

    {Enc,EncT} = tc(fun() -> encode(?times, Module, Type, Value) end),
    {Dec,DecT} = tc(fun() -> decode(?times, Module, Type, Bytes) end),
    sync_tracer(),

    ct:pal(Slogan ++
           "ASN.1 encoding: ~.2f micro (~.1f%), ~.2f micro (~.1f%)~n"
           "ASN.1 decoding: ~.2f micro (~.1f%), ~.2f micro (~.1f%)~n",
           [Module,
            Enc / ?times / ?par, Enc / BaseEnc * 100.0 - 100,
            EncT / ?times / ?par, EncT / BaseEncT * 100.0 - 100,
            Dec/?times / ?par, Dec / BaseDec * 100.0 - 100,
            DecT/?times / ?par, DecT / BaseDecT * 100.0 - 100
           ]).

par(Fun, Procs) ->
    Refs = [spawn_monitor(Fun) || _ <- lists:seq(1,Procs)],
    [receive {'DOWN', Ref, _, Pid, Res} -> Res = normal,Pid end || {_, Ref} <- Refs].

tracer() ->
    spawn(fun() ->
                  catch erlang:process_flag(message_queue_data, off_heap),
                  (fun F(N) -> receive {get, Pid} -> Pid ! N, F(0); _M -> F(N+1) end end)(0)
          end).

sync_tracer() ->
    case erlang:trace_info(self(), tracer) of
        {tracer, Tracer} when is_pid(Tracer) ->
            Tracer ! {get, self()},
            receive N -> N end;
        {tracer, Tracer} when is_port(Tracer) ->
            erlang:port_command(Tracer, term_to_binary({})),
            erlang:port_control(Tracer, $f, <<>>);
        _ -> 0
    end.
