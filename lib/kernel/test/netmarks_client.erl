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

%% This is a TCP client used for network benchmarking.
%% The client sets up connections and writes a fixed amount of data
%% that is expected to be echoed back by the server.

-module(netmarks_client).

-compile(export_all).

-define(DEBUG,1).
-ifdef(DEBUG).
-define(DBG_TRACE(Lvl,F,L), ?DEBUG >= Lvl andalso io:format(F,L)).
-else.
-define(DBG_TRACE(Lvl,F,L),void).
-endif.


-define(DEFAULT_ITERATIONS,(1000*1000)).
-define(DEFAULT_HEADER_SZ,0).
-define(DEFAULT_PACKET_SZ,1000).
-define(DEFAULT_PORT,4999).
-define(DEFAULT_PARALLEL,1).
-define(DEFAULT_HOST,"localhost").
-define(DEFAULT_SYNC,false).
-define(DEFAULT_ACTIVE,once).


run() ->
    run([], gen_tcp).

help() ->
    def_opts().

def_opts() -> % order is significant
    [{parallel,?DEFAULT_PARALLEL},
     {iterations,?DEFAULT_ITERATIONS},
     {host,?DEFAULT_HOST},
     {port,?DEFAULT_PORT},
     {packet_size,?DEFAULT_PACKET_SZ},
     {header_size,?DEFAULT_HEADER_SZ},
     {sync,?DEFAULT_SYNC},
     {active,?DEFAULT_ACTIVE}].

run(Opts, TcpMod) when is_list(Opts) ->
    %%erts_debug:set_internal_state(available_internal_state, true),
    DefOpts = def_opts(),
    verify_opts(Opts, DefOpts),
    [Parallel | RunArgs] = [proplists:get_value(K,Opts,V) || {K,V} <- DefOpts],
    run(Parallel, TcpMod, RunArgs, []).

run(0, _, _, Pids) ->
    TotBytes = join(Pids, 0),
    io:format("Done. Sent and received a total of ~p bytes\n", [TotBytes]);
run(Parallel, TcpMod, Opts, Pids) ->
    Pid = spawn_link(?MODULE,client_start,[self(), TcpMod, Opts]),
    run(Parallel-1, TcpMod, Opts, [Pid | Pids]).

client_start(Papa, TcpMod, [Iters, Host, Port, Psz, Hsz, Sync, Active]) ->
    BuffSize = 1460*2,
    {ok, Sock} =
	TcpMod:connect(Host, Port, [{active,Active}, {packet,Hsz}, binary,
                                    {reuseaddr, true},{recbuf,BuffSize},
                                    {sndbuf,BuffSize}]),
    Bytes = case Sync of
		false ->
		    spawn_link(?MODULE, writer_start,
			       [self(), TcpMod, Sock, Hsz, build_packet(Psz), Iters]),
		    read_loop(TcpMod, Sock, Hsz, 0, undefined, Active);

		true ->
		    sync_loop(TcpMod, Sock, Hsz, build_packet(Psz), Iters, 0, Active)
    end,
    TcpMod:close(Sock),
    case TcpMod of
        sverk_tcp ->
            io:format("sverk_tcp socket stats: ~p\n", [sverk_tcp:stats(Sock)]);
        _ -> ok
    end,
    Papa ! {self(), done, Bytes}.


sync_loop(_, _, _, _, 0, TotBytes, _) ->
    TotBytes;
sync_loop(TcpMod, Sock, Hsz, Packet, Iters, TotBytes0, Active) ->
    TcpMod:send(Sock, Packet),
    Psz = byte_size(Packet),
    ?DBG_TRACE(3,"~p: Sent ~p bytes\n", [self(), Psz]),
    sync_reader(TcpMod, Sock, Psz, Active),
    sync_loop(TcpMod, Sock, Hsz, Packet, Iters-1, TotBytes0 + Psz + Hsz, Active).

sync_reader(_, _Sock, 0, _) -> ok;
sync_reader(TcpMod, Sock, BytesLeft, false) ->
    case TcpMod:recv(Sock,0) of
	{ok, Data} ->
	    Psz = byte_size(Data),
	    sync_reader(TcpMod, Sock, BytesLeft - Psz, false)
    end;
sync_reader(TcpMod, Sock, BytesLeft, Active) ->
    receive
	{tcp, Sock, Data} ->
	    Psz = byte_size(Data),
	    ?DBG_TRACE(3,"~p: Read ~p of ~p bytes\n", [self(), Psz, BytesLeft]),
	    if Active == once -> inet_setopts(Sock,[{active,once}]);
	       true -> ok end,
	    sync_reader(TcpMod, Sock, BytesLeft - Psz, Active);

	Other ->
	    io:format("Reader ~p got unexpected message: ~p\n", [self(), Other]),
	    exit("unexpected msg")
    end.


writer_start(Reader, TcpMod, Sock, Hsz, Packet, Iters) ->
    ?DBG_TRACE(1,"Writer ~p started writing ~p packets of ~p bytes\n",
               [self(), Iters, byte_size(Packet)+Hsz]),
    Written = write_loop(TcpMod, Sock, Hsz, Packet, Iters, 0),
    ?DBG_TRACE(1,"Writer ~p done writing ~p bytes\n", [self(), Written]),
    Reader ! {done_writing, Written}.

write_loop(_, _, _, _, 0, Written) ->
    Written;
write_loop(TcpMod, Sock, Hsz, Packet, N, Written) ->
    ok = TcpMod:send(Sock, Packet),
    write_loop(TcpMod, Sock, Hsz, Packet, N-1, Written+byte_size(Packet)+Hsz).

read_loop(_, _, _, ReadAll, ReadAll, _Active) ->
    ReadAll;
read_loop(TcpMod, Sock, Hsz, ReadBytes0, TotBytes, false) ->
    case TcpMod:recv(Sock, 0, 200) of
	{ok, Data} ->
	    ReadBytes1 = ReadBytes0 + byte_size(Data) + Hsz,
	    ?MODULE:read_loop(TcpMod, Sock, Hsz, ReadBytes1, TotBytes, false);
	{error, closed} ->
            ?DBG_TRACE(1,"Passive reader ~p got 'closed' after reading ~p bytes.\n",
                       [self(), ReadBytes0]),
	    receive
		{done_writing, ReadBytes0} ->
		    undefined = TotBytes,
		    ReadBytes0;
                Other ->
		    io:format("Passive reader ~p got unexpected message after close: ~p\n", [self(), Other]),
		    exit("unexpected msg")
            end;

	{error, timeout} ->
	    receive
		{done_writing, Written} when Written >= ReadBytes0 ->
		    ?DBG_TRACE(1,"Passive reader ~p read ~p bytes of ~p written bytes.\n",
			       [self(), ReadBytes0, Written]),
		    undefined = TotBytes,
		    ?MODULE:read_loop(TcpMod, Sock, Hsz, ReadBytes0, Written, false);
		Other ->
		    io:format("Passive reader ~p (~p bytes read) got unexpected message after timeout: ~p\n",
                              [self(), ReadBytes0, Other]),
		    exit("unexpected msg")
	    after 0 ->
		    ?MODULE:read_loop(TcpMod, Sock, Hsz, ReadBytes0, TotBytes, false)
	    end
    end;
read_loop(TcpMod, Sock, Hsz, ReadBytes0, TotBytes, Active) ->
    receive
        {tcp, Sock, Data} ->
	    ReadBytes1 = ReadBytes0 + byte_size(Data) + Hsz,
	    ?DBG_TRACE(3,"Active reader ~p got ~p bytes, bytes left: ~p\n",
		       [self(), byte_size(Data), bytes_left(ReadBytes1,TotBytes)]),
	    if Active == once -> inet_setopts(Sock,[{active,once}]);
	       true -> ok end,
            ?MODULE:read_loop(TcpMod, Sock, Hsz, ReadBytes1, TotBytes, Active);

	{done_writing, Written} when Written >= ReadBytes0 ->
	    ?DBG_TRACE(1,"Active reader ~p read ~p bytes of ~p written bytes.\n",
		       [self(), ReadBytes0, Written]),
	    undefined = TotBytes,
	    ?MODULE:read_loop(TcpMod, Sock, Hsz, ReadBytes0, Written, Active);

	Other ->
	    io:format("Reader ~p got unexpected message: ~p\n", [self(), Other]),
	    exit("unexpected msg")

    after 1000 ->
	    ?DBG_TRACE(1,"Reader ~p still waiting for ~p bytes\n",
		       [self(), bytes_left(ReadBytes0, TotBytes)]),
            %%erts_debug:get_internal_state(check_io_debug),
	    ?MODULE:read_loop(TcpMod, Sock, Hsz, ReadBytes0, TotBytes, Active)
    end.

bytes_left(_, undefined) -> undefined;
bytes_left(Read, Total) -> Total - Read.

build_packet(Sz) ->
    << <<B>> || B <- packet_bytes(Sz)>>.

packet_bytes(Sz) ->
    packet_bytes($a, Sz, []).

packet_bytes(_, 0, Acc) ->
    lists:reverse(Acc);
packet_bytes($z, Left, Acc) ->
    packet_bytes($a, Left-1, [$z | Acc]);
packet_bytes(Char, Left, Acc) ->
    packet_bytes(Char+1, Left-1, [Char | Acc]).


join([], TotBytes) ->
    TotBytes;
join([Pid | Tail], TotBytes) ->
    receive
	{Pid, done, Bytes} -> join(Tail, TotBytes + Bytes)
    end.

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


inet_setopts({_,_}=SverkSock, Opts) ->
    sverk_tcp:setopts(SverkSock, Opts);
inet_setopts(Sock, Opts) ->
    inet:setopts(Sock, Opts).
