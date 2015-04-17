-module(crash).

%% The program loads a data file ("data.jsn") once and
%% decodes it as json subset repeatedly and checks the
%% strings to be UTF-8, which it is (even ASCII).
%% After some number of iterations, the check crashes.
%% Unfortunately, the behaviour does not reproduce well.
%% The rate of crashes is very sensitive to the contents
%% of the program. Also in some cases the process does not
%% seem to crash at all; we start several parallel Erlang
%% processes (instances) to increase the total probability
%% of a crash.

-export([start/1]).

start(Instance) ->
    {ok,Data} = file:read_file("data.jsn"),
    erlang:garbage_collect(),
    loop_parse(1, Data, Instance).

loop_parse(N, Data, Instance) ->
    try   {ok,_} = parse(Data)
    catch {check_utf8_binary,Key} ->
            io:fwrite("; bug tripped in iteration ~p of instance ~p for Key = ~p.~n",
                      [N, Instance, Key]),
            %% Use with "+Mim true" in crash_it.escript for further information
            %% io:fwrite("instrument:memory_data() ->~n  ~p.~n", [instrument:memory_data()]),
            exit(1)
    end,
    case N rem 100 == 0 orelse N == 1 of
        true -> io:fwrite("; ~b iterations OK of instance ~p.~n",
                          [N, Instance]);
        false -> ok
    end,
    loop_parse(N + 1, Data, Instance).

%% -- stripped down code from our application --

parse(Data) ->
    Lines = binary:split(Data, <<"\n">>, [global, trim]),
    fold_list([], Lines).

fold_list(List, [Utf8|Utf8s]) ->
    {ok,Json} = from_utf8(Utf8),
    fold_list([Json|List], Utf8s);
fold_list(List, []) ->
    {ok,List}.

from_utf8(Utf8) ->
    {ok,Toks} = toks(Utf8, []),
    {ok,Value,[]} = from_toks(Toks),
    {ok,Value}.

from_toks([S|Toks]) when is_binary(S) -> {ok,S,Toks};
from_toks([lbrac|Toks]) -> array_from_toks(Toks, []);
from_toks([lpar|Toks]) -> object_from_toks(Toks, []).

array_from_toks(Toks, RevValues) ->
    case from_toks(Toks) of
        {ok,Value,[comma|Toks1]} -> array_from_toks(Toks1, [Value|RevValues]);
        {ok,Value,[rbrac|Toks1]} -> {ok,lists:reverse([Value|RevValues]),Toks1}
    end.

object_from_toks([K,colon|Toks1], RevKVs) when is_binary(K) ->
    case from_toks(Toks1) of
        {ok,V,[comma|Toks2]} ->
            object_from_toks(Toks2, [{K,V}|RevKVs]);
        {ok,V,[rpar|Toks2]} ->
            {ok,obj([{K,V}|RevKVs]),Toks2}
    end.

toks(<<"[",T/binary>>, Toks) -> toks(T, [lbrac|Toks]);
toks(<<"]",T/binary>>, Toks) -> toks(T, [rbrac|Toks]);
toks(<<"{",T/binary>>, Toks) -> toks(T, [lpar|Toks]);
toks(<<"}",T/binary>>, Toks) -> toks(T, [rpar|Toks]);
toks(<<",",T/binary>>, Toks) -> toks(T, [comma|Toks]);
toks(<<":",T/binary>>, Toks) -> toks(T, [colon|Toks]);
toks(<<"\"",T/binary>>, Toks) -> toks_string(<<>>, T, Toks);
toks(<<>>, Toks) -> {ok,lists:reverse(Toks)}.

toks_string(Acc, <<"\"",T/binary>>, Toks) ->
    toks(T, [Acc|Toks]);
toks_string(Acc, <<C,T/binary>>, Toks) ->
    toks_string(<<Acc/binary,C>>, T, Toks).

obj(KVs) when is_list(KVs) ->
    {obj,from_list([{to_atom(K),V} || {K,V} <- KVs])}.

to_atom(Key) ->
    %%try   ok = check_utf8_binary(binary:copy(Key))
    try   ok = check_utf8_binary(Key)
    catch error:function_clause -> throw({check_utf8_binary,Key})
    end,
    binary_to_atom(Key, utf8).

check_codepoint(Cp) ->
    %% Not all cases required for crash!
    %% The cases are split in separate if-expressions so that they are
    %% not optimized away by the compiler (since 7b10ff7, OTP 18).
    ok = if Cp < 0; Cp >= 16#110000      -> {error,codepoint,Cp};
            true -> ok
         end,
    ok = if 16#d800 =< Cp, Cp =< 16#dfff -> {error,codepoint,Cp};
            true -> ok
         end,
    ok = if 16#fdd0 =< Cp, Cp =< 16#fdef -> {error,codepoint,Cp};
            true -> ok
         end,
    ok = if Cp band 16#ffff >= 16#fffe   -> {error,codepoint,Cp};
            true -> ok
         end.

check_utf8_binary(<<Cp/utf8,More/binary>>) -> % (*)
%check_utf8_binary(<<Cp:8,More/binary>>) -> % (*)
    ok = check_codepoint(Cp),
    check_utf8_binary(More);
check_utf8_binary(<<>>) ->
    ok.

%% -- from orddict.erl before 5a7b211 --

from_list(Pairs) ->
    lists:foldl(fun ({K,V}, D) -> store(K, V, D) end, [], Pairs).

store(Key, New, [{K,_}=E|Dict]) when Key < K ->
    [{Key,New},E|Dict];
store(Key, New, [{K,_}=E|Dict]) when Key > K ->
    [E|store(Key, New, Dict)];
store(Key, New, [{_K,_Old}|Dict]) ->
    [{Key,New}|Dict];
store(Key, New, []) -> [{Key,New}].
