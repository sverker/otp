%%%-------------------------------------------------------------------
%%% File    : rt.erl
%%% Author  : Rickard Green <rickard.s.green@ericsson.com>
%%% Description : Rickards Testning
%%%
%%% Created :  6 Feb 2008 by Rickard Green <rickard.s.green@ericsson.com>
%%%-------------------------------------------------------------------
-module(rt).

-export([r/0,r/1,r/2,r/3,i/0,run/1]).

i() ->
    ts:install().

r() ->
    chk_i(fun () -> ts:run() end).

r(Ts) when is_list(Ts) ->
    R = lists:map(fun (T) ->
			  chk_i(fun () -> ts:run(T, [batch]) end)
		  end, Ts),
    search_core_files(),
    R;
r(T) when is_atom(T) ->
    R = chk_i(fun () -> ts:run(T, [batch]) end),
    search_core_files(),
    R.

r(A, Ss) when is_atom(A), is_list(Ss) ->
    R = lists:map(fun (S) ->
			  chk_i(fun () -> ts:run(A, S, [batch]) end)
		  end, Ss),
    search_core_files(),
    R;
r(A, S) when is_atom(A), is_atom(S) ->
    R = chk_i(fun () -> ts:run(A, S, [batch]) end),
    case {A, S} of
	{system, Z} when Z == z; Z == z_SUITE -> ok;
	_ -> search_core_files()
    end,
    R.

r(A, S, T) when is_atom(A), is_atom(S), is_atom(T) ->
    R = chk_i(fun () -> ts:run(A, S, T, [batch]) end),
    case {A, S, T} of
	{system, z_SUITE, core_files} -> ok;
	_ -> search_core_files()
    end,
    R;
r(A, S, {group,G}=T) when is_atom(A), is_atom(S), is_atom(G) ->
    R = chk_i(fun () -> ts:run(A, S, T, [batch]) end),
    search_core_files(),
    R.

% for use with -s
run([T]) -> r(T);
run([A,S]) -> r(A,S);
run([A,S,T]) -> r(A,S,T).

chk_i(Fun) ->
    case Fun() of
	{error, R} when R == not_installed;
			R == inconsistent_platforms ->
	    i(),
	    Fun();
	Res ->
	    Res
    end.

search_core_files() ->
    catch code:purge(z_SUITE),
    case catch code:load_abs(filename:join(["..","system_test","z_SUITE"])) of
	{module,z_SUITE} ->
	    catch z_SUITE:search_for_core_files("..");
	_ -> io:format("Cannot search for cores since z_SUITE isn't found.~n",
		       [])
    end.
