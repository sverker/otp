%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2012. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%

%% This is a TCP server used for network benchmarking.
%% The server accepts connections and echoes back all received data
%% until the client closes a connection.

-module(netmarks_server).

-compile(export_all).

%-define(DEBUG,true).
-ifdef(DEBUG).
-define(DBG_TRACE(F,L),io:format(F,L)).
-else.
-define(DBG_TRACE(F,L),void).
-endif.

-define(DEFAULT_PORT,4999).
-define(DEFAULT_HEADER_SZ,0).
-define(DEFAULT_ACTIVE,once).

run() ->
    run([]).

help() ->
    def_opts().

def_opts() ->
    [{port, ?DEFAULT_PORT},
     {header_size, ?DEFAULT_HEADER_SZ},
     {active, ?DEFAULT_ACTIVE},
     {master, none}].

run(Opts) when is_list(Opts) ->
    io:format("netmarks_server:run called in ~p with Opts = ~p\n",[self(), Opts]),
    DefOpts = def_opts(),
    verify_opts(Opts, DefOpts),
    RunArgs = [proplists:get_value(K,Opts,V) || {K,V} <- DefOpts],
    run_it(RunArgs);

run(Port) when is_integer(Port) ->
    run([{port,Port}]).

run_it([Port, Packet, Active, Master]) ->
    {ok,LSock} =
	gen_tcp:listen(Port,[{active,Active},{packet,Packet},
			     {reuseaddr, true},{backlog,100}, binary]),
    case Master of
	Pid when is_pid(Pid) ->
	    Master ! {self(), ready_to_serve};
	none ->
	    ok
    end,
    accept_loop(LSock,Active).

accept_loop(LSock,Active) ->
    case gen_tcp:accept(LSock) of
	{ok, Sock} ->
	    Ref = make_ref(),
	    Pid = spawn(?MODULE,server_start,[Sock,Ref,Active]),
	    gen_tcp:controlling_process(Sock, Pid),
	    Pid ! Ref,
	    accept_loop(LSock,Active);
	Other ->
	    io:format("Accept returned ~p, exit.~n",[Other])
    end.


server_start(Sock, Ref, Active) ->
    receive Ref -> ok
    end,
    case Active of
	once -> server_loop_active_once(Sock);
	true -> server_loop_active(Sock);
	false -> server_loop_passive(Sock)
    end.

server_loop_active_once(S) ->
    receive
        {tcp,S,Data} ->
	    ?DBG_TRACE("Read ~w bytes\n",[byte_size(Data)]),
            Answer = process(Data),
            gen_tcp:send(S,Answer),
	    inet:setopts(S,[{active,once}]),
            ?MODULE:server_loop_active_once(S);
        {tcp_closed,S} ->
            io:format("Socket ~w closed [~w]~n",[S,self()]),
            ok
    end.

server_loop_active(S) ->
    receive
        {tcp,S,Data} ->
	    ?DBG_TRACE("Read ~w bytes\n",[byte_size(Data)]),
            Answer = process(Data),
            gen_tcp:send(S,Answer),
            ?MODULE:server_loop_active(S);
        {tcp_closed,S} ->
            io:format("Socket ~w closed [~w]~n",[S,self()]),
            ok
    end.

server_loop_passive(S) ->
    case gen_tcp:recv(S,0) of
        {ok,Data} ->
	    ?DBG_TRACE("Read ~w bytes\n",[byte_size(Data)]),
            Answer = process(Data),
            gen_tcp:send(S,Answer),
            ?MODULE:server_loop_passive(S);
        {error,closed} ->
            io:format("Socket ~w closed [~w]~n",[S,self()]),
            ok
    end.

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
