-module(netmarks_udp_server).

-compile(export_all).

-define(DEFAULT_PORT,4999).
-define(DEFAULT_ACTIVE,false).

run() ->
    run([]).

help() ->
    def_opts().

def_opts() ->
    [{port, ?DEFAULT_PORT},
     {active, ?DEFAULT_ACTIVE},
     {master, none},
     {listeners, 1}].


run(Opts) when is_list(Opts) ->
    DefOpts = def_opts(),
    verify_opts(Opts, DefOpts),
    RunArgs = [proplists:get_value(K,Opts,V) || {K,V} <- DefOpts],
    run_it(RunArgs).

run_it([Port, Active, Master, Listeners]) ->
    Self = self(),
    Pids = [element(1,spawn_monitor(fun() -> udp_server(Self,Port+N,Active) end))
	    || N <- lists:seq(0,Listeners-1)],

    wait_for_ready(Pids),
    case Master of
	_ when is_pid(Master) ->
	    Master ! {self(), ready_to_serve};
	none -> ok
    end,

    wait_for_exit(Pids).

wait_for_ready([]) -> ok;
wait_for_ready(Pids) ->
    receive
	{Pid, is_ready} ->
	    wait_for_ready(lists:delete(Pid,Pids))
    end.

wait_for_exit([]) -> ok;
wait_for_exit(Pids) ->
    receive
	{'DOWN',_,_,Pid,_} ->
	    wait_for_exit(lists:delete(Pid,Pids))
    end.

udp_server(Papa, Port, Active) ->
    {ok, Sock} = gen_udp:open(Port,[{active,false},{reuseaddr, true}, binary,
				    {read_packets,1000},
				    {recbuf,100000}, {sndbuf,100000}]),
    random:seed(),
    Papa ! {self(), is_ready},
    case Active of
	false -> server_loop_passive(Sock);
	once -> server_loop_active_once(Sock)
    end.

server_loop_active_once(S) ->
    inet:setopts(S,[{active,once}]),
    receive
        {udp,S,IP,Port,Data} ->
	    case random:uniform(10) of
		100 ->
		    io:format("Dropping!~n",[]);
		_ ->
	            Answer = process(Data),
		    gen_udp:send(S,IP,Port,Answer)
	    end
    end,
    ?MODULE:server_loop_active_once(S).

server_loop_passive(S) ->
    {ok, {IP,Port,Data}} = gen_udp:recv(S,1000000),
    case random:uniform(10) of
	100 ->
	    io:format("Dropping!~n",[]);
	_ ->
	    Answer = process(Data),
	    gen_udp:send(S,IP,Port,Answer)
    end,
    ?MODULE:server_loop_passive(S).


process(<<_I:32/big,_/binary>> = Data) ->
    %%io:format("~p~n",[I]),
    Data;
process(Data) ->
    Data.


verify_opts([], _) ->
    ok;
verify_opts([{Key,_} | Tail], DefOpts) ->
    case proplists:is_defined(Key, DefOpts) of
	true -> ok;
	false ->
	    Reason = lists:flatten(io_lib:format("Unknown option '~p'", [Key])),
	    exit(Reason)
    end,
    verify_opts(Tail, DefOpts).
