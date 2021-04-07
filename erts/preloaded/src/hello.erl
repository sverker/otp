%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2021. All Rights Reserved.
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
-define(RUN_ESTONE, true).

-module(hello).
-export([test/0, start/0, start/2, undefined_function/3, id/1]).

%% Internal exports.
-export([process_main/1]).

%% Internal exports for estone.
-if(?RUN_ESTONE).
-export([lists/1,
	 msgp/1,
	 msgp_medium/1,
	 msgp_huge/1,
	 pattern/1,
	 trav/1,
	 large_dataset_work/1,
	 large_local_dataset_work/1,mk_big_procs/1,big_proc/0, very_big/1,
	 alloc/1,
	 bif_dispatch/1,
	 binary_h/1,echo/1,
	 ets/1,
	 generic/1,req/2,gserv/4,handle_call/3,
	 int_arith/1,
	 float_arith/1,
	 fcalls/1,remote0/1,remote1/1,app0/1,app1/1,
	 timer/1,
	 links/1,lproc/1,
         run_micro/2,p1/1,ppp/3,ppp_loop/3,macro/1,micros/0]).
-endif.

test() ->
    L = [<<"-root">>,<<"/home/user/git/otp">>,<<"-progname">>,<<"/home/user/git/otp/bin/cerl -debug +asmdump">>,<<"--">>,<<"-home">>,<<"/home/user">>,<<"--">>,<<"-kernel">>,<<"shell_history">>,<<"enabled">>,<<"--">>,<<"--">>],
    start(?MODULE, L).

start(_BootMod, BootArgs) ->
    _ = id(BootArgs),
    erlang:display_string("hello, world\n"),
    erlang:display_string(id("arguments: ")),
    erlang:display(id(BootArgs)),
    erlang:display_string("Testing stuff (should not crash)...\n"),
    test(BootArgs),
    erlang:display_string("Everything is fine!\n"),
    estone(),
    halt(42, []).

%% Dummy process entry point for processes that must always be running.
start() ->
    receive
        Any -> Any
    end.

-define(EARLY_DAYS, false).

-if(?EARLY_DAYS).
%% This definition will do if test_apply_errors/1 is commented out.
undefined_function(_, _, _) -> ok.
-else.
%% This definition is needed for test_apply_errors/1 to succeed.
undefined_function(Module, Func, Args) ->
    Tuple = {Module,Func,Args,[]},
    try erlang:error(undef)
    catch
	error:undef:Stacktrace ->
	    Stk = [Tuple|tl(Stacktrace)],
	    erlang:raise(error, undef, Stk)
    end.
-endif.

id(I) ->
    I.

test(BootArgs) ->
    external_call(),
    BootArgs = rev_test(BootArgs),
    reductions(BootArgs),
    list_test(),
    tuple_test(),
    test_guard_tests(BootArgs),
    preserving_xregs(),
    swapping(),
    apply_test(),
    guard_bif_test(),
    test_catches(),
    test_try_catch(),
    test_return_yielding(),
    test_jmp(),
    test_length(BootArgs),
    test_float(),
    test_funs(),

    %% From this point on, EARLY_DAYS must be set false so that
    %% it is possible to define an error_handler similar to the
    %% error_handler in the KERNEL application.
    test_apply_errors(),
    test_fun_call_errors(),
    test_processes(),
    test_set_tuple_element(),
    test_tagged_tuple(),
    test_select_val(),
    test_external_call_errors(),
    test_is_lt(),
    test_is_ge(),
    test_is_ne_exact(),
    test_receive_timeout(),
    test_selective_receive(),
    test_error_action_code(),
    test_trace_breakpoint(),
    test_load_nif(),

    ok.

%% Test external calls, as well as '!', receive, self/0, and node/0.
external_call() ->
    self() ! call_ext,
    call_ext = ?MODULE:start(),

    self() ! call_ext_only,
    call_ext_only = external_call_only(),

    external_call_last = external_call_last(),

    self() ! node(),
    Node = node(),
    Node = id(?MODULE:start()),

    ok.

external_call_only() ->
    ?MODULE:start().

external_call_last() ->
    self() ! external_call_last,
    ?MODULE:start().

rev_test(L0) ->
    L = rev(L0),
    L0 = rev(L).

rev(L) -> rev(L, []).

rev([H|T], Acc) -> rev(T, [H|Acc]);
rev([], Acc) -> Acc.

list_test() ->
    [a|_] = id([a]),
    [_|[b]] = id([a,b]),

    ok.

tuple_test() ->
    T = id({a,b,c}),
    if is_tuple(T) -> ok end,
    {A,B,C} = T,
    [a,b,c] = [A,B,C],

    {tag,xyz,b,42,w,x,y,z} = build_tuple(id(tag), id(b), id(42), id(w), id(x), id(y), id(z)),

    {tag,X,Y} = id({tag,x,y}),
    [x,y] = [X,Y],
    ok = case id({short}) of
             {tag,_,_} -> error;
             _ -> ok
         end,
    ok = case id({wrong_tag,x,y}) of
             {tag,_,_} -> error;
             _ -> ok
         end,

    %% Build a tuple and put into an Y register.
    Tuple = {id(a),b,c},
    _ = id(0),
    {a,b,c} = id(Tuple),

    id(ok).

%% Also test proper handling of X registers not backed by ARM registers.
build_tuple(Tag, A, B, C, D, E, F) ->
    {id(Tag),xyz,A,B,C,D,E,F}.

test_guard_tests(BootArgs) ->
    test_numeric_guards(),

    true = is_atom(id(a)),
    false = is_atom(id(42)),

    true = is_tuple(id({})),

    true = is_list(id([])),
    true = is_list(id([a])),
    false = is_list(id({a,b})),
    false = is_list(id(42)),

    true = is_boolean(id(false)),
    true = is_boolean(id(true)),
    false = is_boolean(id(a)),
    false = is_boolean(id(42)),
    false = is_boolean({id(a),b}),

    test_boot_args(BootArgs),

    BigNum = id(16#fffffffffffffffffffffffffffff),
    {Sub1,Sub2} = split_binary(id(<<"abcde">>), 3),
    true = is_binary(id(<<"abc">>)),
    true = is_binary(list_to_binary(id([]))),
    true = is_binary(list_to_binary(BootArgs)),
    true = is_binary(id(Sub1)),
    true = is_binary(id(Sub2)),
    false = is_binary(id(a)),
    false = is_binary(id(<<1:1>>)),
    false = is_binary(BigNum),

    true = is_bitstring(id(<<"abc">>)),
    true = is_bitstring(list_to_binary(id([]))),
    true = is_bitstring(list_to_binary(BootArgs)),
    true = is_bitstring(id(Sub1)),
    true = is_bitstring(id(Sub2)),
    true = is_bitstring(id(<<1:1>>)),
    false = is_bitstring(id(a)),
    false = is_bitstring(id(BigNum)),

    true = is_pid(id(self())),
    true = is_pid(external_pid()),
    false = is_pid(BigNum),
    false = is_pid(Sub1),
    false = is_pid(id(a)),
    false = is_pid(BootArgs),

    true = is_reference(make_ref()),
    true = is_reference(external_ref()),
    false = is_reference(external_pid()),
    false = is_reference(BigNum),
    false = is_reference(Sub1),
    false = is_reference(id(a)),
    false = is_reference(BootArgs),

    true = is_map(id(#{})),
    true = is_map(id(#{a => b})),
    false = is_map(BigNum),
    false = is_map(Sub1),
    false = is_map(id(a)),
    false = is_map(BootArgs),

    ok.

external_pid() ->
    B = <<131,88,100,0,11,97,114,110,101,64,115,101,108,100,111,
          110,0,0,0,50,0,0,0,0,96,96,14,170>>,
    binary_to_term(B).

external_ref() ->
    B = <<131,90,0,3,100,0,11,97,114,110,101,64,122,97,112,104,111,
          100,96,96,14,188,0,1,106,122,169,196,0,1,6,56,120,157>>,
    binary_to_term(B).

test_boot_args([H|T]) when is_binary(H) ->
    test_boot_args(T);
test_boot_args([]) -> ok.

test_numeric_guards() ->
    true = is_integer(id(42)),
    true = is_integer(id(16#7777777777777777777777777777777777)),
    false = is_integer(id(0.5)),
    false = is_integer(id(a)),
    false = is_integer(id({a,b})),
    false = is_integer(id([a,b])),

    true = is_number(id(42)),
    true = is_number(id(16#7777777777777777777777777777777777)),
    true = is_number(id(0.5)),
    false = is_number(id(a)),
    false = is_float(id({a,b})),
    false = is_number(id([a,b])),

    false = is_float(id(42)),
    false = is_float(id(16#7777777777777777777777777777777777)),
    true = is_float(id(0.5)),
    false = is_float(id(a)),
    false = is_float(id({a,b})),
    false = is_float(id([a,b])),

    ok.

preserving_xregs() ->
    Args = get_args(?FUNCTION_NAME),
    {A,B,C,D,E,F} = Args,
    Args = do_preserving(A, B, C, D, E, F),
    ok.

do_preserving(A, B, C, D, E, F) ->
    if
        E =/= F ->
            {A,B,C,D,E,F}
    end.

swapping() ->
    Args = get_args8(?FUNCTION_NAME),
    {A,B,C,D,E,F,G,H} = Args,
    {B,C,D,E,F,G,H,A} = swapping_1(1, 2, 3, A, B, C, D, E, F, G, H),
    {B,A,F,D,E,C,H,G} = swapping_2(1, 2, 3, A, B, C, D, E, F, G, H),
    {A,B,C,D,E,H,G,F} = swapping_3(1, 2, 3, A, B, C, D, E, F, G, H).

swapping_1(X, Y, Z, A, B, C, D, E, F, G, H) ->
    swapping_build(X, Y, Z, B, C, D, E, F, G, H, A).

swapping_2(X, Y, Z, A, B, C, D, E, F, G, H) ->
    swapping_build(X, Y, Z, B, A, F, D, E, C, H, G).

swapping_3(X, Y, Z, A, B, C, D, E, F, G, H) ->
    swapping_build(X, Y, Z, A, B, C, D, E, H, G, F).

swapping_build(_, _, _, A, B, C, D, E, F, G, H) ->
    {A,B,C,D,E,F,G,H}.

get_args(Tag) ->
    get_args6(Tag).

get_args6(Tag) ->
    {{Tag,a},{Tag,b},{Tag,make_ref()},[Tag,d],{Tag,e},[Tag,f]}.

get_args8(Tag) ->
    list_to_tuple([{Tag,1},{Tag,2}|tuple_to_list(get_args6(Tag))]).

reductions(L0) ->
    L1 = red(L0, L0, []),
    L2 = red(L0, L1, []),
    L2 = rev(rev(L2)),
    ok.

red([_|T], L, Acc) ->
    red(T, L, L++Acc);
red([], _, Acc) ->
    Acc.

apply_test() ->
    Mod = id(?MODULE),
    Id = id(id),
    {a,b} = Mod:Id({a,b}),
    {x,y} = apply_fixed_last(Mod, Id, {x,y}),

    Args = id([{a,b,c}]),
    {a,b,c} = apply(Mod, Id, Args),
    {a,b,c} = apply_var_last(Mod, Id, Args),
    {a,b,c} = apply_var_only(Mod, Id, Args),

    ok.

apply_fixed_last(M, F, Arg) ->
    M:F(Arg).

apply_var_last(M, F, Args) ->
    _ = id(42),
    apply(M, F, Args).

apply_var_only(M, F, Args) ->
    apply(M, F, Args).

guard_bif_test() ->
    P = id(3.14),
    Bin = id(<<1,2,3,4,5,6>>),
    3 = floor(P),

    T = id({a,b,c}),
    b = element(2, T),

    <<3,4,5>> = binary_part(Bin, 2, 3),

    Tuple = id(get_args8(?FUNCTION_NAME)),
    if element(1, Tuple) =:= {?FUNCTION_NAME,1} -> ok end,

    gbif_preserving_xregs(Tuple),
    gbif_nofail(Tuple).

%% Test that all X registers are preserved when calling a guard BIF.
gbif_preserving_xregs(Tuple) ->
    X = id(-42),
    {A,B,C,D,E,F,G,H} = id(Tuple),

    %% We must prevent the compiler from delaying the extraction
    %% of the tuple elements in the tuple matching above by using
    %% all extracted elements.
    1 = element(2, A),
    2 = element(2, B),
    a = element(2, C),
    b = element(2, D),
    2 = tuple_size(E),
    d = hd(tl(F)),
    2 = tuple_size(G),
    f = hd(tl(H)),

    if
        %% Test that guard BIFs called in a guard preserve all X registers.
        abs(X) =:= 42 ->
            %% Rebuild the tuple from the extracted X registers and match.
            Tuple = {A,B,C,D,E,F,G,H}
    end,

    ok.

%% Test guard BIFs that can't fail.
gbif_nofail(Tuple0) ->
    Tuple = id(Tuple0),

    true = id(is_tuple(Tuple)),
    false = id(is_list(Tuple)),

    true = id(Tuple0 =:= Tuple),
    false = id(Tuple =:= 42),
    ok.

test_catches() ->
    thrown = (catch do_throw(id(thrown))),

    {'EXIT',{{badmatch,b},[_|_]}} =
    id(catch a = id(b)),
    {'EXIT',whatever} = (catch exit(id(whatever))),

    22 = (catch id(22)),

    ok.

do_throw(What) ->
    throw(What).

test_try_catch() ->
    try foobar(id({a,b}), id({x,y})) of
        _ ->
            error(expected_failure)
    catch
        error:function_clause:Stk1 ->
            [{?MODULE,foobar,[{a,b},{x,y}],_},{?MODULE,test_try_catch,0,_}|_] = Stk1
    end,

    try implicit_rethrow() of
        _ ->
            error(expected_failure)
    catch
        error:something_wrong ->
            ok
    end,

    try explicit_rethrow() of
        _ ->
            error(expected_failure)
    catch
        error:something_wrong ->
            ok
    end,

    badarg = try
                 foobar(error, something_wrong)
             catch error:Reason:Stk2 ->
                     %% Test a raw_raise with an error return of badarg.
                     erlang:raise(id(bad_class), Reason, Stk2)
             end,

    try try_clause() of
        _ ->
            error(expected_failure)
    catch
        error:{try_clause,{sum,12}}:Stk3 ->
            [{?MODULE,try_clause,0,_}|_] = Stk3
    end,

    ok = case_clause(42),
    try case_clause({id(p),q}) of
        _ ->
            error(expected_failure)
    catch
        error:{case_clause,{p,q}}:Stk4 ->
            [{?MODULE,case_clause,1,_}|_] = Stk4
    end,

    ok = if_clause(true),
    try if_clause(other) of
        _ ->
            error(expected_failure)
    catch
        error:if_clause:Stk5 ->
            [{?MODULE,if_clause,1,_}|_] = Stk5
    end,

    try id(1 bsl (1 bsl 1024)) of
        _ ->
            error(expected_failure)
    catch
        error:system_limit ->
            ok
    end,

    ok.

implicit_rethrow() ->
    try
        foobar(error, something_wrong)
    catch throw:not_matching ->
            %% Will not match. The exception will be implicitly rethrown
            %% using a 'raise' instruction.
            ok
    end.

explicit_rethrow() ->
    try
        foobar(error, something_wrong)
    catch error:Reason:Stk ->
            %% Explicitly rethrow the exception, using the instruction
            %% 'raw_raise'.
            erlang:raise(error, Reason, Stk)
    end.

try_clause() ->
    try foobar(3, 9) of
        {sum,not_the_sum_we_are_looking_for} ->
            ok
    catch
        _:_ ->
            error
    end.

case_clause(Arg) ->
    case Arg of
        42 -> ok
    end.

if_clause(Arg) ->
    if
        Arg -> ok
    end.

foobar(A, B) when is_integer(A + B) ->
    {sum,A + B};
foobar(error, Reason) ->
    error(Reason).

test_return_yielding() ->
    12502500 = ret_yield(5000).

ret_yield(0) -> 0;
ret_yield(N) -> N + ret_yield(N - 1).

test_jmp() ->
    Arg = id(true),
    Result = if
                 Arg ->
                     ok;
                 true ->
                     error
             end,
    ok = id(Result).

test_length(L) ->
    N = len(L),
    N = length(L),

    LongLen = 4000*16,
    LongList = seq(1, LongLen),
    LongLen = len(LongList),
    LongLen = length(LongList),

    BadList = append(LongList, bad_tail),

    try length(BadList) of
        _ ->
            error(expected_failure)
    catch
        error:badarg:Stk ->
            [{erlang,length,[BadList],_},{?MODULE,test_length,1,_}|_] = Stk
    end,

    if
        length(LongList) =:= LongLen ->
            ok
    end,

    error = if
                length(BadList) =:= 0 ->
                    ok;
                true ->
                    error
            end,

    ok.

len(L) ->
    len(L, 0).

len([_|T], N) -> len(T, N + 1);
len([], N) -> N.

append([H|T], Tail) ->
    append(T, [H|Tail]);
append([], Tail) ->
    Tail.

seq(First, Last) ->
    seq_loop(Last-First+1, Last, []).

seq_loop(0, _, L) ->
    L;
seq_loop(N, X, L) ->
    seq_loop(N - 1, X, [X|L]).

test_float() ->
    Float = id(42.0),
    Zero = id(0.0),

    true = is_float(Float),
    true = is_float(Zero),

    Mul3 = Float * 3,
    0.0 = Mul3 * Zero,
    42.0 = Mul3 / 3.0,
    126.0 = Mul3,

    Div3 = Float / 3.0,
    42.0 = Div3 * 3.0,
    14.0 = Div3,

    Add3 = Float + 3.0,
    Add3 = Add3 + Zero,
    42.0 = Add3 - 3.0,
    45.0 = Add3,

    Sub3 = Float - 3.0,
    Sub3 = Sub3 - Zero,
    42.0 = Sub3 + 3.0,
    39.0 = Sub3,

    Neg = -Float,
    42.0 = -Neg,
    -42.0 = Neg,

    {'EXIT', {badarith,_}} = (catch Float / Zero),

    ok.

test_funs() ->
    F = fun(X) -> X + 42 end,
    44 = F(2),
    50 = tail_call_fun(F),

    AF1 = adder(1),
    AF2 = adder(10),
    3 = AF1(2),
    9 = tail_call_fun(AF1),
    109 = AF2(99),

    MA = mul_adder(3, 7),
    10 = MA(1),
    13 = MA(2),
    31 = tail_call_fun(MA),

    45 = apply(F, id([3])),
    37 = apply_fun_last(MA, id([10])),
    28 = apply_fun_only(MA, id([7])),

    ok = (id(fun() -> ok end))(),

    ok.

adder(N) ->
    fun(X) -> N + X end.

mul_adder(F, N) ->
    fun(X) -> F * X + N end.

tail_call_fun(F) ->
    F(8).

apply_fun_last(F, Args) ->
    _ = id(42),
    apply(F, Args).

apply_fun_only(F, Args) ->
    apply(F, Args).

test_apply_errors() ->
    M = id(bad_module),

    %% If running hello as the only one module in the system,
    %% there is no error_handler module. Set up this module
    %% as the error_handler module to ensure that the we
    %% always get the same type of `undef` exception.
    OldErrorHandler = erlang:process_flag(error_handler, ?MODULE),

    try M:whatever(42) of
        _ ->
            error(expected_failure)
    catch
        error:undef:Stk1 ->
            [{M,whatever,[42],_}|_] = Stk1
    end,

    try apply(M, whatever, id([42])) of
        _ ->
            error(expected_failure)
    catch
        error:undef:Stk2 ->
            [{M,whatever,[42],_}|_] = Stk2
    end,

    erlang:process_flag(error_handler, OldErrorHandler),
    ok.

test_fun_call_errors() ->
    F = id(bad_fun),
    G = id(fun() -> ok end),

    try F(42) of
        _ ->
            error(expected_failure)
    catch
        error:{badfun,bad_fun}:Stk1 ->
            [{?MODULE,test_fun_call_errors,0,_}|_] = Stk1
    end,

    Arg = {id(a),b},
    try G(Arg) of
        _ ->
            error(expected_failure)
    catch
        error:{badarity,{G,[Arg]}}:Stk2 ->
            [{?MODULE,test_fun_call_errors,0,_}|_] = Stk2
    end,

    ok.

test_processes() ->
    start_logger(),
    ExitData = {data,make_ref()},
    normal = process_call(fun() -> ok end),
    ExitData = process_call(fun() -> exit(ExitData) end),

    {badarg,[{erlang,process_flag,[_,_],_}|_]} =
        process_call(fun() ->
                             %% Fail in a BIF.
                             process_flag(self(), trap_exit)
                     end),

    {{badmatch,b},[{?MODULE,_,_,_}|_]} =
        process_call(fun() ->
                             a = id(b)
                     end),

    normal = process_call(fun test_continue_exit/0),

    erlang:system_flag(system_logger, logger),
    ok.

my_spawn_opt(F, Opt) ->
    spawn_opt(?MODULE, process_main, [F], Opt).

process_call(F) ->
    {Pid,Ref} = spawn_opt(?MODULE, process_main, [F], [monitor]),
    receive
        {'DOWN',Ref,process,Pid,ExitData} ->
            ExitData
    end.

process_main(F) ->
    F().

start_logger() ->
    Logger = my_spawn_opt(fun logger_main/0, [link]),
    erlang:system_flag(system_logger, Logger),
    ok.

logger_main() ->
    receive
        _Any ->
            %% erlang:display(_Any),
            logger_main()
    end.

test_continue_exit() ->
    T = ets:new(foo, []),
    insert_a_lot(T, 10_000),
    ok.

insert_a_lot(_, 0) ->
    ok;
insert_a_lot(T, N) ->
    ets:insert(T, {N,value}),
    insert_a_lot(T, N - 1).

-record(huge, {f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,
               f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,
               f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,
               f31,f32,f33,f34,f35,f36,f37,f38,f39,f40}).

test_set_tuple_element() ->
    Huge = id(#huge{}),

    #huge{f31={u,31},f32={u,32}} = update31(Huge),
    #huge{f32={u,32},f33={u,33}} = update32(Huge),
    #huge{f33={u,33},f34={u,34}} = update33(Huge),
    #huge{f34={u,34},f35={u,35}} = update34(Huge),

    ok.

update31(#huge{}=Huge) ->
    Huge#huge{f31={u,31},f32={u,32}}.

update32(#huge{}=Huge) ->
    Huge#huge{f32={u,32},f33={u,33}}.

update33(#huge{}=Huge) ->
    Huge#huge{f33={u,33},f34={u,34}}.

update34(#huge{}=Huge) ->
    Huge#huge{f34={u,34},f35={u,35}}.

%% Test i_is_tagged_tuple_ff.
test_tagged_tuple() ->
    a = error_info(id({error_info,a})),
    false = error_info(id(false)),
    {a,b} = error_info(id({a,b})),
    other = error_info(id(other)),
    ok.

error_info(Arg) ->
    case Arg of
        {error_info,Info} ->
            Info;
        false ->
            false;
        Other ->
            Other
    end.

test_select_val() ->
    1 = sel_atom5(id(a)),
    2 = sel_atom5(id(b)),
    3 = sel_atom5(id(c)),
    4 = sel_atom5(id(d)),
    5 = sel_atom5(id(e)),

    6 = sel_atom(id(f)),

    1 = sel_atom(id(a)),
    2 = sel_atom(id(b)),
    3 = sel_atom(id(c)),
    4 = sel_atom(id(d)),
    5 = sel_atom(id(e)),
    6 = sel_atom(id(f)),
    7 = sel_atom(id(g)),
    8 = sel_atom(id(h)),
    9 = sel_atom(id(i)),
    10 = sel_atom(id(j)),
    11 = sel_atom(id(k)),

    5 = select_tuple_arity({id(2),3}),
    9 = select_tuple_arity({id(2),3,4}),
    error = select_tuple_arity(id({a})),
    error = select_tuple_arity(id(1 bsl 128)),
    error = select_tuple_arity(id(a)),

    ok.

sel_atom5(a) -> 1;
sel_atom5(b) -> 2;
sel_atom5(c) -> 3;
sel_atom5(d) -> 4;
sel_atom5(e) -> 5.

sel_atom(a) -> 1;
sel_atom(b) -> 2;
sel_atom(c) -> 3;
sel_atom(d) -> 4;
sel_atom(e) -> 5;
sel_atom(f) -> 6;
sel_atom(g) -> 7;
sel_atom(h) -> 8;
sel_atom(i) -> 9;
sel_atom(j) -> 10;
sel_atom(k) -> 11.

select_tuple_arity({A,B}) ->
    A + B;
select_tuple_arity({A,B,C}) ->
    A + B + C;
select_tuple_arity(_) ->
    error.

test_external_call_errors() ->
    try no_module:no_function({id(ok),42}) of
        _ ->
            error(expected_failure)
    catch
        error:undef:Stk1 ->
            [{no_module,no_function,_,_}|_] = Stk1
    end,

    OldErrorHandler = erlang:process_flag(error_handler, ?MODULE),

    try no_module:no_function({id(ok),42}) of
        _ ->
            error(expected_failure)
    catch
        error:undef:Stk2 ->
            [{no_module,no_function,[{ok,42}],_}|_] = Stk2
    end,

    try tail_call_external_error({id(ok),100}) of
        _ ->
            error(expected_failure)
    catch
        error:undef:Stk3 ->
            [{no_module,no_function,[{ok,100}],_}|_] = Stk3
    end,

    erlang:process_flag(error_handler, OldErrorHandler),
    ok.

tail_call_external_error(Arg) ->
    no_module:no_function(Arg).

test_is_lt() ->
    true = id(4) < 5,
    true = id(1) < 16#07ff_ffff_ffff_ffff,
    true = id(-2) < 1,
    true = id(-1) < 1,
    false = id(42) < 42,

    true = 4 < id(5),
    true = 1 < id(16#07ff_ffff_ffff_ffff),
    true = -2 < id(1),
    true = -1 < id(1),
    false = 42 < id(42),

    true = id(4) < id(5),
    true = id(1) < id(16#07ff_ffff_ffff_ffff),
    true = id(-2) < id(1),
    true = id(-1) < id(1),
    false = id(42) < id(42),

    %% Atoms.
    A = id(a),
    B = id(b),

    true = A < b,
    true = a < B,
    true = A < B,

    false = A < a,
    false = a < A,
    false = A < A,
    false = id(a) < A,

    test_is_lt_atoms(A, B, id(c), id(d), id(e), id(f), id(g), id(h)),

    false = B < a,
    false = B < A,
    false = B < A,

    %% Floats.
    Two = id(2.0),
    Three = id(3.0),

    true = 2.0 < Three,
    true = Two < 3.0,
    true = Two < Three,

    false = 3.0 < Two,
    false = Three < 2.0,
    false = Three < Two,

    false = 2.0 < Two,
    false = Two < 2.0,
    false = Two < Two,
    false = id(2.0) < Two,

    %% Mixed.
    true = Two < 3,
    true = 2 < Three,
    true = id({a,b}) < id({x,y}),

    true = Two < {a,b},
    false = [a,b] < Two,

    Args = get_args8(?FUNCTION_NAME),
    {V1,V2,V3,V4,V5,V6,V7,V8} = Args,
    test_is_lt_mixed(V1, V2, V3, V4, V5, V6, V7, V8),

    ok.

%% Test that high-numbered X registers work/are not clobbered.
test_is_lt_atoms(V1, V2, V3, V4, V5, V6, V7, V8) ->
    true = V1 < V2,
    true = V3 < V4,
    true = V5 < V6,
    true = V6 < V7,
    false = V8 < V7.

%% Test that high-numbered X registers work/are not clobbered.
test_is_lt_mixed(V1, V2, V3, V4, V5, V6, V7, V8) ->
    true = V1 < V2,
    true = V3 < V4,
    false = V5 < V5,
    false = V6 < V7,                         %Tuple is less than list.
    true = V7 < V8,
    ok.

test_is_ge() ->
    false = id(4) >= 5,
    false = id(1) >= 16#07ff_ffff_ffff_ffff,
    true = 16#07ff_ffff_ffff_ffff >= id(1),
    false = id(-2) >= 1,
    false = id(-1) >= 1,
    true = id(42) >= 42,

    false = 4 >= id(5),
    false = 1 >= id(16#07ff_ffff_ffff_ffff),
    false = -2 >= id(1),
    false = -1 >= id(1),
    true = 42 >= id(42),

    false = id(4) >= id(5),
    false = id(1) >= id(16#07ff_ffff_ffff_ffff),
    false = id(-2) >= id(1),
    false = id(-1) >= id(1),
    true = id(42) >= id(42),

    %% Mixed.
    Two = id(2.0),
    Three = id(3.0),
    false = Two >= 3,
    false = 2 >= Three,
    false = id({a,b}) >= id({x,y}),

    false = Two >= {a,b},
    true = [a,b] >= Two,

    Args = get_args8(?FUNCTION_NAME),
    {V1,V2,V3,V4,V5,V6,V7,V8} = Args,
    test_is_ge_mixed(V1, V2, V3, V4, V5, V6, V7, V8),

    ok.

%% Test that high-numbered X registers work/are not clobbered.
test_is_ge_mixed(V1, V2, V3, V4, V5, V6, V7, V8) ->
    false = V1 >= V2,
    false = V3 >= V4,
    true = V5 >= V5,
    true = V6 >= V7,                         %Tuple is less than list.
    false = V7 >= V8,
    ok.

test_is_ne_exact() ->
    ok = ne_exact(id(0), id([])),
    ok = ne_exact(id(42), id([42])),
    error = ne_exact(id(42), id([42,43])),
    ok.

ne_exact(0, _) ->
    ok;
ne_exact(_, [_]) ->
    ok;
ne_exact(_, [_|_]) ->
    error.

test_receive_timeout() ->
    receive after 1 -> ok end,

    try
        receive after id(bad_timeout) -> ok end,
        error(expected_failure)
    catch
        error:timeout_value:Stk ->
            [{?MODULE,?FUNCTION_NAME,0,_}|_] = Stk
    end,

    ok.

test_selective_receive() ->
    %% A selective receive will test the loop_rec_end instruction.
    Self = self(),
    Echo = my_spawn_opt(fun() ->
                                Self ! {self(),started},
                                echo_server_loop()
                       end, [link]),

    receive
        {Echo,started} -> ok
    end,

    Echo ! {echo,Self,{id(a),b}},
    receive {a,b} -> ok end,

    Echo ! {echo,Self,whatever},
    receive whatever -> ok end,

    Echo ! stop,

    ok.

echo_server_loop() ->
    receive
        {echo,Pid,Msg} ->
            Pid ! Msg,
            echo_server_loop();
        stop ->
            ok
    end.

test_error_action_code() ->
    Body2 = fun() ->
                    exit(bad)
            end,
    Body1 = fun() ->
                    my_spawn_opt(Body2, [link]),
                    receive _ -> ok end
            end,
    process_flag(trap_exit, true),
    Pid = my_spawn_opt(Body1, [link]),
    receive
        {'EXIT',Pid,bad} ->
            ok
    end,
    process_flag(trap_exit, false),

    ok.

test_trace_breakpoint() ->
    erts_internal:trace_pattern({hello,id,1}, true, [local]),
    42 = id(42),
    erts_internal:trace_pattern({hello,id,1}, false, [local]),
    17 = id(17),
    ok.

test_load_nif() ->
    prim_file:on_load().

%%%
%%% Here follows Estone, extracted from estone_SUITE.
%%%

-if(not ?RUN_ESTONE).
estone() -> ok.
-else.

%% EStone defines
-define(TOTAL, (3000 * 1000 * 100)).   %% 300 secs
-define(BIGPROCS, 2).
-define(BIGPROC_SIZE, 50).
-define(STONEFACTOR, 31000000).   %% Factor to make the reference
                             %% implementation to make 1000 TS_ESTONES.
-record(micro,
	{function, %% The name of the function implementing the micro
	 weight,   %% How important is this in typical applications ??
	 loops = 100,%% initial data
	 tt1,      %% time to do one round
	 str}).    %% Header string

estone() ->
    OldErrorHandler = erlang:process_flag(error_handler, ?MODULE),
    erlang:display_string("\nRunning estone...\n"),
    Micros = micros(),
    L = macro(Micros),
    {_Total, Stones} = sum_micros(L, 0, 0),
    erlang:display_string(integer_to_list(Stones) ++ " ESTONES\n"),
    erlang:process_flag(error_handler, OldErrorHandler),
    ok.

sum_micros([], Tot, Stones) -> {Tot, Stones};
sum_micros([H|T], Tot, Sto) ->
    sum_micros(T, ks(microsecs, H) + Tot, ks(estones, H) + Sto).

ks(K, L) ->
    {value, {_, V}} = lists:keysearch(K, 1, L),
    V.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EStone test
micro(lists) ->
     #micro{function = lists,
	    weight = 7,
	    loops = 6400,
	    str = "list manipulation"};
micro(msgp) ->
    #micro{function = msgp,
	    weight = 10,
	    loops = 1515,
	    str = "small messages"};
micro(msgp_medium) ->
    #micro{function = msgp_medium,
	    weight = 14,
	    loops = 1527,
	    str = "medium messages"};
micro(msgp_huge) ->
    #micro{function = msgp_huge,
	    weight = 4,
	    loops = 52,
	    str = "huge messages"};

micro(pattern) ->
    #micro{function = pattern,
	    weight = 5,
	    loops = 1046,
	    str = "pattern matching"};

micro(trav) ->
    #micro{function = trav,
	    weight = 4,
	    loops = 2834,
	    str = "traverse"};

micro(large_dataset_work) ->
    #micro{function = large_dataset_work,
	   weight = 3,
	   loops = 1193,
	   str = "Work with large dataset"};

micro(large_local_dataset_work) ->
    #micro{function = large_local_dataset_work,
	   weight = 3,
	   loops = 1174,
	   str = "Work with large local dataset"};

micro(alloc) ->
    #micro{function = alloc,
	   weight = 2,
	   loops = 3710,
	   str = "Alloc and dealloc"};

micro(bif_dispatch) ->
    #micro{function = bif_dispatch,
	   weight = 8,
	   loops = 5623,
	   str = "Bif dispatch"};

micro(binary_h) ->
    #micro{function = binary_h,
	   weight = 4,
	   loops = 581,
	   str = "Binary handling"};
micro(ets) ->
    #micro{function = ets,
	   weight = 6,
	   loops = 342,
	   str = "ets datadictionary"};
micro(generic) ->
    #micro{function = generic,
	   weight = 9,
	   loops = 7977,
	   str = "Generic server (with timeout)"};
micro(int_arith) ->
    #micro{function = int_arith,
	   weight = 3,
	   loops = 4157,
	   str = "Small Integer arithmetics"};
micro(float_arith) ->
    #micro{function = float_arith,
	   weight = 1,
	   loops = 5526,
	   str = "Float arithmetics"};
micro(fcalls) ->
    #micro{function = fcalls,
	   weight = 5,
	   loops = 882,
	   str = "Function calls"};

micro(timer) ->
    #micro{function = timer,
	   weight = 2,
	   loops = 2312,
	   str = "Timers"};

micro(links) ->
    #micro{function = links,
	   weight = 1,
	   loops = 30,
	   str = "Links"}.



%% Return a list of micro's
micros() ->
    [
     micro(lists),
     micro(msgp),
     micro(msgp_medium),
     micro(msgp_huge),
     micro(pattern),
     micro(trav),
     micro(large_dataset_work),
     micro(large_local_dataset_work),
     micro(alloc),
     %%micro(bif_dispatch),
     micro(binary_h),
     micro(ets),
     micro(generic),
     micro(int_arith),
     micro(float_arith),
     micro(fcalls),
     micro(timer),
     micro(links)
    ].

macro(Ms) ->
    statistics(reductions),
    statistics(runtime),
    lists(500),                           % fixup cache on first round
    run_micros(Ms).

run_micros([]) ->
    erlang:display_string("\n"),
    [];
run_micros([H|T]) ->
    R = run_micro(H),
    [R| run_micros(T)].

run_micro(M) ->
    Pid = spawn(?MODULE, run_micro, [self(),M]),
    Res = receive {Pid, Reply} -> Reply end,
    {value,{title,Title}} = lists:keysearch(title,1,Reply),
    {value,{estones,Estones}} = lists:keysearch(estones,1,Reply),
    erlang:display({Title,Estones}),
    Res.

run_micro(Top, M) ->
    Top ! {self(), apply_micro(M)}.

apply_micro(M) ->
    statistics(reductions),
    Before = monotonic_time(),
    Compensate = apply_micro(M#micro.function, M#micro.loops),
    After = monotonic_time(),
    {_, Reds} = statistics(reductions),
    Elapsed = subtr(Before, After),
    MicroSecs = Elapsed - Compensate,
    [{title, M#micro.str},
     {tt1, M#micro.tt1},
     {function, M#micro.function},
     {weight_percentage, M#micro.weight},
     {loops, M#micro.loops},
     {microsecs,MicroSecs},
     {estones, (M#micro.weight * M#micro.weight * ?STONEFACTOR) div max(1,MicroSecs)},
     {kilo_reductions, Reds div 1000}
    ].

max(A, B) when A < B -> B;
max(A, _) -> A.

monotonic_time() ->
    try erlang:monotonic_time() catch error:undef -> erlang:now() end.

subtr(Before, After) when is_integer(Before), is_integer(After) ->
    convert_time_unit(After-Before, native, 1000000);
subtr({_,_,_}=Before, {_,_,_}=After) ->
    now_diff(After, Before).

now_diff({A2, B2, C2}, {A1, B1, C1}) ->
    ((A2-A1)*1000000 + B2-B1)*1000000 + C2-C1.

convert_time_unit(Time, FromUnit, ToUnit) ->
    try
	FU = case FromUnit of
		 native -> erts_internal:time_unit();
                 perf_counter -> erts_internal:perf_counter_unit();
		 nanosecond -> 1000*1000*1000;
		 microsecond -> 1000*1000;
		 millisecond -> 1000;
		 second -> 1;

		 %% Deprecated symbolic units...
		 nano_seconds -> 1000*1000*1000;
		 micro_seconds -> 1000*1000;
		 milli_seconds -> 1000;
		 seconds -> 1;

		 _ when FromUnit > 0 -> FromUnit
	     end,
	TU = case ToUnit of
		 native -> erts_internal:time_unit();
                 perf_counter -> erts_internal:perf_counter_unit();
		 nanosecond -> 1000*1000*1000;
		 microsecond -> 1000*1000;
		 millisecond -> 1000;
		 second -> 1;

		 %% Deprecated symbolic units...
		 nano_seconds -> 1000*1000*1000;
		 micro_seconds -> 1000*1000;
		 milli_seconds -> 1000;
		 seconds -> 1;

		 _ when ToUnit > 0 -> ToUnit
	     end,
	case Time < 0 of
	    true -> TU*Time - (FU - 1);
	    false -> TU*Time
	end div FU
    catch
	_ : _ ->
	    error(badarg, [Time, FromUnit, ToUnit])
    end.

tc(M, F, A) ->
    T1 = erlang:monotonic_time(),
    Val = apply(M, F, A),
    T2 = erlang:monotonic_time(),
    Time = convert_time_unit(T2 - T1, native, microsecond),
    {Time, Val}.

duplicate(N, X) when is_integer(N), N >= 0 -> duplicate(N, X, []).

duplicate(0, _, L) -> L;
duplicate(N, X, L) -> duplicate(N-1, X, [X|L]).

apply_micro(Name, Loops) ->
    erlang:display({Name,Loops}),
    apply(?MODULE, Name, [Loops]).

%%%%%%%%%%%% micro bench manipulating lists. %%%%%%%%%%%%%%%%%%%%%%%%%
lists(I) ->
    L1 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    L2 = "aaaaaaaaaa",
    lists(I, L1, L2).

lists(0, _,_) ->
    0;
lists(I, L1, L2) ->
    revt(10, L1),
    appt(10, L1, L2),
    lists(I-1, L1, L2).

revt(0, _) ->
    done;
revt(I, L) ->
    reverse(L),
    revt(I-1, L).

reverse(L) ->
    reverse(L, []).
reverse([H|T], Ack) -> reverse(T, [H|Ack]);
reverse([], Ack) -> Ack.

estone_append([H|T], L) ->
    [H | estone_append(T, L)];
estone_append([], L) ->
    L.

appt(0, _L1, _L2) -> ok;
appt(I, L1, L2) ->
    estone_append(L1, L2),
    appt(I-1, L1, L2).


%%%%%%%%%%%%%%% small message passing and ctxt switching %%%%%%%
msgp(I) ->
    msgp(I, small()).

msgp(0, _) ->
    0;
msgp(I, Msg) ->
    P1 = spawn(?MODULE, p1, [self()]),
    P2 = spawn(?MODULE, p1, [P1]),
    P3 = spawn(?MODULE, p1, [P2]),
    P4 = spawn(?MODULE, p1, [P3]),
    msgp_loop(100, P4, Msg),
    msgp(I-1, Msg).

p1(To) ->
    receive
	{_From, {message, X}} ->
	    To ! {self(), {message, X}},
	    p1(To);
	stop ->
	    To ! stop,
	    exit(normal)
    end.

msgp_loop(0, P, _) ->
    P ! stop,
    receive
	stop -> ok
    end;
msgp_loop(I, P, Msg) ->
    P ! {self(), {message, Msg}},
    receive
	{_From, {message, _}} ->
	    msgp_loop(I-1, P, Msg)
    end.

%%%%%%%%%%%% large massage passing and ctxt switching %%%%%%%
msgp_medium(I) ->
        msgp_medium(I, big()).

msgp_medium(0, _) ->
    0;
msgp_medium(I, Msg) ->
    P1 = spawn(?MODULE , p1, [self()]),
    P2 = spawn(?MODULE, p1, [P1]),
    P3 = spawn(?MODULE, p1, [P2]),
    P4 = spawn(?MODULE, p1, [P3]),
    msgp_loop(100, P4, Msg),
    msgp_medium(I-1, Msg).



%%%%%%%%%%%% huge massage passing and ctxt switching %%%%%%%
msgp_huge(I) ->
        msgp_huge(I, very_big(15)).

msgp_huge(0, _) ->
    0;
msgp_huge(I, Msg) ->
    P1 = spawn(?MODULE , p1, [self()]),
    P4 = spawn(?MODULE, p1, [P1]),
    msgp_loop(100, P4, Msg),
    msgp_huge(I-1, Msg).


%%%%%% typical protocol pattern matching %%%%%%%
pattern(0) ->
    0;
pattern(I) ->
    Tail = "aaabbaaababba",
    P1 = [0, 1,2,3,4,5|Tail],
    pat_loop1(100, P1),
    pat_loop2(100, P1),
    pat_loop3(100, P1),
    pat_loop4(100, P1),
    pat_loop5(100, P1),
    pattern(I-1).

pat_loop1(0, _) ->
    ok;
pat_loop1(_I, [_, _X, _Y, 0 |_T])  ->
    ok;
pat_loop1(_I, [_, _X, _Y, 1| _T]) ->
    ok;
pat_loop1(_I, [_, _X, _Y, 2 | _T]) ->
    ok;
pat_loop1(I, [_, X, Y, 3 | T]) ->
    pat_loop1(I-1, [0, X,Y,3|T]).

pat_loop2(0, _) ->
    ok;
pat_loop2(_I, [_X, Y | _Tail]) when Y bsl 1 == 0 ->
    ok;
pat_loop2(_I, [_X, Y | _Tail]) when Y bsl 2 == 0 ->
    ok;
pat_loop2(I, [X, Y | Tail]) when Y bsl 2 == 4 ->
    pat_loop2(I-1, [X, Y |Tail]).


pat_loop3(0, _) ->
    ok;
pat_loop3(_I, [{c, h} | _Tail]) ->
    ok;
pat_loop3(_I, [1, 0 |_T]) ->
    ok;
pat_loop3(_I, [X, _Y |_Tail]) when is_binary(X), size(X) == 1 ->
    ok;
pat_loop3(_I, [no, _Y|_Tail]) ->
    ok;
pat_loop3(_I, []) ->
    ok;
pat_loop3(_I, [X,_Y|_T]) when X /= 0 ->
    ok;
pat_loop3(_I, [2,3|_T]) ->
    ok;
pat_loop3(_I, [1, 2]) ->
    ok;
pat_loop3(I, [0, 1 |T]) ->
    pat_loop3(I-1, [0,1|T]).


pat_loop4(0, _) ->  ok;
pat_loop4(_I, [20|_T]) -> ok;
pat_loop4(_I, [219|_T]) -> ok;
pat_loop4(_I, [18|_T]) -> ok;
pat_loop4(_I, [17|_T]) -> ok;
pat_loop4(_I, [16|_T]) -> ok;
pat_loop4(_I, [15|_T]) -> ok;
pat_loop4(_I, [14|_T]) -> ok;
pat_loop4(_I, [13|_T]) -> ok;
pat_loop4(_I, [12|_T]) -> ok;
pat_loop4(_I, [11|_T]) -> ok;
pat_loop4(_I, [10|_T]) -> ok;
pat_loop4(_I, [9|_T]) -> ok;
pat_loop4(_I, [8|_T]) -> ok;
pat_loop4(_I, [7|_T]) -> ok;
pat_loop4(_I, [6|_T]) -> ok;
pat_loop4(_I, [5|_T]) -> ok;
pat_loop4(_I, [4|_T]) -> ok;
pat_loop4(_I, [3|_T]) -> ok;
pat_loop4(_I, [1|_T]) -> ok;
pat_loop4(_I, [21|_T]) -> ok;
pat_loop4(_I, [22|_T]) -> ok;
pat_loop4(_I, [23|_T]) -> ok;
pat_loop4(_I, [24|_T]) -> ok;
pat_loop4(_I, [25|_T]) -> ok;
pat_loop4(_I, [26|_T]) -> ok;
pat_loop4(_I, [27|_T]) -> ok;
pat_loop4(I, [0|T]) ->
    pat_loop4(I-1, [0|T]).

pat_loop5(0, _) -> ok;
pat_loop5(_I, [0, 20|_T]) -> ok;
pat_loop5(_I, [0, 19|_T]) -> ok;
pat_loop5(_I, [0, 18|_T]) -> ok;
pat_loop5(_I, [0, 17|_T]) -> ok;
pat_loop5(_I, [0, 16|_T]) -> ok;
pat_loop5(_I, [0, 15|_T]) -> ok;
pat_loop5(_I, [0, 14|_T]) -> ok;
pat_loop5(_I, [0, 13|_T]) -> ok;
pat_loop5(_I, [0, 12|_T]) -> ok;
pat_loop5(_I, [0, 11|_T]) -> ok;
pat_loop5(_I, [0, 10|_T]) -> ok;
pat_loop5(_I, [0, 9|_T]) -> ok;
pat_loop5(_I, [0, 8|_T]) -> ok;
pat_loop5(_I, [0, 7|_T]) -> ok;
pat_loop5(_I, [0, 6|_T]) -> ok;
pat_loop5(I, [0, 1|T]) ->
    pat_loop5(I-1, [0,1|T]).

%%%%%%%%%% term traversal representing simple pattern matchhing %%%
%%%%%%%%%                              + some arith
trav(I) ->
    X = very_big(10),
    trav(I, X).

trav(0, _) -> 0;
trav(I, T) ->
    do_trav(T),
    trav(I-1, T).

do_trav(T) when is_tuple(T) ->
    tup_trav(T, 1, 1 + size(T));
do_trav([H|T]) ->
    do_trav(H) + do_trav(T);
do_trav(X) when is_integer(X) -> 1;
do_trav(_X) -> 0.
tup_trav(_T, P, P) -> 0;
tup_trav(T, P, End) ->
    do_trav(element(P, T)) + tup_trav(T, P+1, End).

ppp(Top, I, EstoneCat) ->
    P = open_port({spawn, EstoneCat}, []),%% cat sits at the other end
    Str = duplicate(200, 88), %% 200 X'es
    Cmd = {self(), {command, Str}},
    receive
	go -> ok
    end,
    ppp_loop(P, I, Cmd),
    Cmd2 = {self(), {command, "abcde"}},
    Res = ppp_loop(P, I, Cmd2),
    P ! {self(), close},
    receive
	{P, closed} ->
	    closed
    end,
    Top ! {self(), Res}.

ppp_loop(_P, 0, _) ->
    ok;
ppp_loop(P, I, Cmd) ->
    P ! Cmd,
    receive
	{P, _} ->  %% no match
	    ppp_loop(P, I-1, Cmd)
    end.

%% Working with a very large non-working data set
%% where the passive data resides in remote processes
large_dataset_work(I) ->
    {Minus, Ps} = tc(?MODULE, mk_big_procs, [?BIGPROCS]),
    trav(I),
    lists(I),
    send_procs(Ps, stop),
    Minus. %% Don't count time to create the big procs.

mk_big_procs(0) -> [];
mk_big_procs(I) ->
    [ mk_big_proc()| mk_big_procs(I-1)].

mk_big_proc() ->
    P = spawn(?MODULE, big_proc, []),
    P ! {self(), running},
    receive
	{P, yes} -> P
    end.

big_proc() ->
    X = very_big(?BIGPROC_SIZE), %% creates a big heap
    Y = very_big(?BIGPROC_SIZE),
    Z = very_big(?BIGPROC_SIZE),

    receive
	{From, running} ->
	    From ! {self(), yes}
    end,
    receive
	stop ->
	    {X, Y, Z}  %% Can't be garbed away now by very (not super)
                       %% smart compiler
    end.

%% Working with a large non-working data set
%% where the data resides in the local process.
large_local_dataset_work(I) ->
    {Minus, _Data} = tc(?MODULE, very_big, [?BIGPROC_SIZE]),
    trav(I),
    lists(I),
    Minus.


%% Fast allocation and also deallocation that is gc test
%% Important to not let variable linger on the stack un-necessarily
alloc(0) -> 0;
alloc(I) ->
    _X11 = very_big(),
    _X12 = very_big(),
    _X13 = very_big(),
    _Z = [_X14 = very_big(),
	  _X15 = very_big(),
	  _X16 = very_big()],
    _X17 = very_big(),
    _X18 = very_big(),
    _X19 = very_big(),
    _X20 = very_big(),
    _X21 = very_big(),
    _X22 = very_big(),
    _X23 = very_big(),
    _X24 = very_big(),
    alloc(I-1).

%% Time to call bif's
%% This benchmark was updated in OTP-24. I've tried to keep the
%% number of stones is creates the same, but that is impossible
%% to achieve across all platforms.
bif_dispatch(0) ->
    0;
bif_dispatch(I) ->
    put(mon,erlang:monitor(process,self())),
    disp(),    disp(),    disp(),    disp(),    disp(),    disp(),
    disp(),    disp(),    disp(),    disp(),    disp(),    disp(),
    bif_dispatch(I-1).

disp() ->
    erts_debug:flat_size(true),
    erts_debug:size_shared(true),
    demonitor(get(mon)),
    erts_debug:flat_size(true),
    demonitor(get(mon)),
    erts_debug:size_shared(true),
    demonitor(get(mon)),
    erts_debug:flat_size(true),
    demonitor(get(mon)),
    erts_debug:size_shared(true),
    demonitor(get(mon)),
    erts_debug:flat_size(true),
    demonitor(get(mon)),
    erts_debug:size_shared(true),
    demonitor(get(mon)),
    erts_debug:flat_size(true),
    demonitor(get(mon)),
    erts_debug:size_shared(true),
    demonitor(get(mon)),
    erts_debug:flat_size(true),
    demonitor(get(mon)),
    erts_debug:size_shared(true).

%% Generic server like behaviour
generic(I) ->
    register(funky, spawn(?MODULE, gserv, [funky, ?MODULE, [], []])),
    g_loop(I).

g_loop(0) ->
    exit(whereis(funky), kill),
    0;
g_loop(I) ->
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [xyz]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [xyz]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    g_loop(I-1).

req(Name, Req) ->
    R = make_ref(),
    Name ! {self(), R, Req},
    receive
	{Name, R, Reply} -> Reply
    after 2000 ->
	    exit(timeout)
    end.

gserv(Name, Mod, State, Debug) ->
    receive
	{From, Ref, {call, Req}} when Debug == [] ->
	    case catch apply(Mod, handle_call, [From, State, Req]) of
		{reply, Reply, State2} ->
		    From ! {Name, Ref, Reply},
		    gserv(Name, Mod, State2, Debug);
		{noreply, State2} ->
		    gserv(Name, Mod, State2, Debug);
		{'EXIT', Reason} ->
		    exit(Reason)
	    end;
	{_From, _Ref, _Req} when Debug /= [] ->
	    exit(nodebug)
    end.

handle_call(_From, _State, [xyz]) ->
    R = atom_to_list(xyz),
    {reply, R, []};
handle_call(_From, State, [abc]) ->
    R = 1 + 3,
    {reply, R, [R | State]}.



%% Binary handling, creating, manipulating and sending binaries
binary_h(I) ->
    Before = monotonic_time(),
    P = spawn(?MODULE, echo, [self()]),
    B = list_to_binary(duplicate(2000, 5)),
    After = monotonic_time(),
    Compensate = subtr(Before, After),
    binary_h_2(I, P, B),
    Compensate.

binary_h_2(0, P, _B) ->
    exit(P, kill);
binary_h_2(I, P, B) ->
    echo_loop(P, 20, B),
    split_loop(B, {abc,1,2222,self(),"ancnd"}, 100),
    binary_h_2(I-1, P, B).

split_loop(_B, _, 0) ->
    ok;
split_loop(B, Term, I) ->
    {X, Y} = split_binary(B, I),
    size(X),
    binary_to_list(Y, 1, 2),
    binary_to_term(term_to_binary(Term)),
    split_loop(B, Term, I-1).


echo_loop(_P, 0, _B) ->
    k;
echo_loop(P, I, B) ->
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    echo_loop(P, I-1, B).


ets(0) ->
    0;
ets(I) ->
    T1 = ets:new(a, [set]),
    T2 = ets:new(c, [bag, private]),
    L = [T1, T2],
    run_tabs(L, L, 1),
    ets:delete(T1),
    ets:delete(T2),
    ets(I-1).

run_tabs(_, _, 0) ->
    ok;
run_tabs([], L, I) ->
    run_tabs(L, L, I-1);
run_tabs([Tab|Tail], L, I) ->
    Begin = I * 20,
    End = (I+1) * 20,
    run_tab(Tab, Begin, End, I),
    run_tabs(Tail, L, I).

run_tab(_Tab, X, X, _) ->
    ok;
run_tab(Tab, Beg, End, J) ->
    ets:insert(Tab, {Beg, J}),
    ets:insert(Tab, {J, Beg}),
    ets:insert(Tab, {{foo,Beg}, J}),
    ets:insert(Tab, {{foo, J}, Beg}),
    ets:delete(Tab, haha),
    match_delete(Tab, {k, j}),
    ets:match(Tab, {Beg, '$1'}),
    ets:match(Tab, {'$1', J}),
    ets:delete(Tab, Beg),
    K = ets:first(Tab),
    _K2 = ets:next(Tab, K),
    run_tab(Tab, Beg+1, End, J).

match_delete(Table, Pattern) ->
    select_delete(Table, [{Pattern,[],[true]}]),
    true.

select_delete(Tab, [{'_',[],[true]}]) ->
    ets:internal_delete_all(Tab, undefined);
select_delete(Tab, MatchSpec) ->
    ets:internal_select_delete(Tab, MatchSpec).

%%%% Integer arith %%%%%
int_arith(0) ->
    0;
int_arith(I) ->
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
    do_arith(I) +
	66,
    int_arith(I-1).

do_arith(I) ->
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
	99.

do_arith2(I) ->
    X = 23,
    _Y = 789 + I,
    Z = I + 1,
    U = (X bsl 1 bsr I) * X div 2 bsr 4,
    U1 = Z + Z + Z + Z + X bsl 4 * 2 bsl 2,
    Z - U + U1 div 2.


%%%% Float arith %%%%%
float_arith(0) ->
    0;
float_arith(I) ->
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
    f_do_arith(I) +
	66,
    float_arith(I-1).

f_do_arith(I) ->
    X = 23.4,
    _Y = 789.99 + I,
    Z = I + 1.88,
    U = (X * 1 / I) * X / 2 * 4,
    U1 = Z + Z + Z + Z + X * 4 * 2 / 2,
    Z - U + U1 / 2.

%%%% time to do various function calls
fcalls(0) ->
    0;
fcalls(I) ->
    local0(400),
    remote0(400),
    app0(400),
    local1(400),
    remote1(400),
    app1(400),
    fcalls(I-1).


local0(0) -> 0;
local0(N) ->
    local0(N-1).

local1(0) -> 0;
local1(N) ->
    1+local1(N-1).

remote0(0) -> 0;
remote0(N) ->
    ?MODULE:remote0(N-1).

remote1(0) -> 0;
remote1(N) ->
    1+?MODULE:remote1(N-1).

app0(0) -> 0;
app0(N) ->
    apply(?MODULE, app0, [N-1]).

app1(0) -> 0;
app1(N) ->
    1 + apply(?MODULE, app1, [N-1]).

%%%%%% jog the time queue implementation
timer(I) ->
    L = [50, 50, 50, 100, 1000, 3000, 8000, 50000, 100000],
    timer(I, L).

timer(0, _) -> 0;
timer(N, L) ->
    send_self(100),
    recv(100,L, L),
    timer(N-1).

recv(0, _, _) ->
    ok;
recv(N, [], L) ->
    recv(N, L, L);
recv(N, [Timeout|Tail], L) ->
    receive
        hi_dude ->
            recv(N-1, Tail, L)
    after Timeout ->
            erlang:display_string("XXXXX this wasn't supposed to happen???\n"),
            ok
    end.

send_self(0) ->
    ok;
send_self(N) ->
    self() ! hi_dude,
    send_self(N-1).


%%%%%%%%%%%% managing many links %%%%%
links(I) ->
    L = mk_link_procs(100),
    send_procs(L, {procs, L, I}),
    wait_for_pids(L),
    0.

mk_link_procs(0) ->
    [];
mk_link_procs(I) ->
    [spawn_link(?MODULE, lproc, [self()]) | mk_link_procs(I-1)].


lproc(Top) ->
    process_flag(trap_exit,true),
    receive
	{procs, Procs, I} ->
	    Top ! {self(), lproc(Procs, Procs, link, I)}
    end.

lproc(_, _, _, 0) ->
    done;
lproc([], Procs, link, I) ->
    lproc(Procs, Procs, unlink, I-1);
lproc([], Procs, unlink, I) ->
    lproc(Procs, Procs, link, I-1);
lproc([Pid|Tail], Procs, unlink, I) ->
    unlink(Pid),
    lproc(Tail, Procs, unlink, I);
lproc([Pid|Tail], Procs, link, I) ->
    link(Pid),
    lproc(Tail, Procs, unlink, I).



%%%%%%%%%%% various utility functions %%%%%%%

echo(Pid) ->
    receive
	X -> Pid ! X,
	     echo(Pid)
    end.

very_big() ->
    very_big(2).
very_big(0) -> [];
very_big(I) ->
    {1,2,3,a,v,f,r,t,y,u,self(), self(), self(),
     "22222222222222222", {{"234", self()}},
     [[very_big(I-1)]]}.

big() ->
    {self(), funky_stuff, baby, {1, [123, true,[]], "abcdef"}}.

small() -> {self(), true}.

%% Wait for a list of children to respond
wait_for_pids([]) ->
    ok;
wait_for_pids([P|Tail]) ->
    receive
	{P, _Res} -> wait_for_pids(Tail)
    end.

send_procs([P|Tail], Msg) -> P ! Msg, send_procs(Tail, Msg);
send_procs([], _) -> ok.

-endif.
