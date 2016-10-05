%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2002-2012. All Rights Reserved.
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

-module(netmarks_SUITE).
%% Test functions
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2,
	 c_to_c/1,c_to_erlang/1,erlang_to_c/1,erlang_to_erlang/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

-export([gen_tcp/1]).
-export([sverk_tcp/1]).

%% Remote host variable
-define(remote_host, "NETMARKS_REMOTE_HOST").

%% Internal
-export([exec_start/3]).

%%-include_lib("test_server/include/test_server.hrl").
-include_lib("common_test/include/ct_event.hrl").

%% Test suite defines
-define(default_timeout, {minutes,10}).

-define(config(P,Conf), proplists:get_value(P, Conf)).

%% EStone defines
-define(MICROS_STONE_DIVIDENT, (6*1000*1000*1000)).
-define(THRPUT_STONE_DIVISOR, (10*1000)).
-define(NUM_REPS,2).

suite() -> [{ct_hooks,[ts_install_cth]},
	    {timetrap,?default_timeout}].

all() ->
    [sverker,
     gen_tcp,
     {group,netmarks_bench},
     {group,netmarks_parallel}
    ].

groups() ->
    [{netmarks_parallel, [{repeat,?NUM_REPS}],
      [{group,local},{group,remote}]},
     {netmarks_bench, [{repeat,?NUM_REPS}],
      [{group,local},{group,remote}]},

     {remote,[],[{group,udp},
		 {group,tcp}]},
     {local,[],[{group,tcp}]},

     {tcp,[],[{group,latency},
	      {group,throughput}]},
     {latency,[],[{group,active_false},
		  {group,active_once}]},
     {throughput,[],[{group,active_false},
		     {group,active_once}]},
     {active_true,[],netmarks()},
     {active_false,[],netmarks()},
     {active_once,[],netmarks()},
     {udp,[],[c_to_erlang]}
    ].

sverk_tcp(Config) ->
    sverker(Config, sverk_tcp).

gen_tcp(Config) ->
    sverker(Config, gen_tcp).

sverker(Config, TcpMod) ->
    sverk_tcp:init(?config(data_dir,Config)),
    C1 = init_per_group(local, Config),
    C2 = init_per_group(throughput, C1),
    C3 = init_per_group(active_once, C2),
    C4 = init_per_group(netmarks_parallel, C3),
    run_benchmark(C4, TcpMod, c).

netmarks() ->
    [
     c_to_c,
     c_to_erlang,
     erlang_to_c,
     erlang_to_erlang
    ].

init_per_suite(Config) ->
    NodeArgs = "-pa " ++ filename:dirname(code:which(?MODULE)),
    io:format("NodeArgs = ~p.~n", [NodeArgs]),

    Prog =
	case os:find_executable("erl") of
	    false -> "erl";
	    P -> P
	end,
    io:format("Prog = ~p.~n", [Prog]),

    {ok, Host} = inet:gethostname(),
    [{benchmark_opts,[]},
     {node_args, NodeArgs},
     {prog, Prog},
     {host,Host} | Config].

end_per_suite(_Config) ->
    ok.

init_per_group(netmarks_parallel, Config) ->
    [{suite,netmarks_parallel_SUITE},
     add_benchmark_opts([{parallel,20}],Config)|Config];
init_per_group(remote, Config) ->
    case os:getenv(?remote_host) of
	false ->
	    {skip, "No "++?remote_host++" configured"};
	RemoteHost ->
	    [{type,remote},{itermod,0.1}|
	     init_host(init_host([{host,RemoteHost}|Config], remote),local)]
    end;
init_per_group(local, Config) ->
    [{type,local},{itermod,1}|init_host(Config, local)];
init_per_group(latency, Config) ->
    I = round(200000 * ?config(itermod, Config)),
    [{benchmark_name, "Latency"},
     add_benchmark_opts([{sync,true}, {iterations,I}],Config)|Config];
init_per_group(throughput, Config) ->
    I = round(1000*1000 * ?config(itermod, Config)),
    [{benchmark_name, "Throughput"},
     add_benchmark_opts([{sync,false},{iterations,I}],Config)|Config];
init_per_group(active_once, Config) ->
    [{active, once} | Config];
init_per_group(active_false, Config) ->
    [{active, false} | Config];
init_per_group(active_true, Config) ->
    [{active, true} | Config];
init_per_group(udp, Config) ->
    [{protocol, udp}, {active,once},
     {benchmark_name,"UDP_Throughput"},
     add_benchmark_opts([{iterations,100000}],Config)| Config];

init_per_group(_, Config) ->
    Config.

init_host(Config, local = ConnType) ->
    [{host_tag(ConnType), ?config(host,Config)},
     {node_tag(ConnType), node()} | Config];
init_host(Config, ConnType) ->
    NodeArgs = ?config(node_args, Config),
    Prog = ?config(prog, Config),
    ServerHost = ?config(host, Config),
    {ok, ServerNode} =
	slave:start(ServerHost, node_name(ConnType), NodeArgs, no_link, Prog),
    pong = net_adm:ping(ServerNode),
    [{host_tag(ConnType), ServerHost},
     {node_tag(ConnType), ServerNode} | Config].

end_per_group(remote,Config) ->
    slave:stop(?config(node_tag(remote), Config)),
    end_per_group(local, Config);
end_per_group(_, _Config) ->
    ok.

init_per_testcase(c_to_c, Config) ->
    case ?config(active, Config) of
	once -> Config;
	_Else -> {skip,"Ignore duplicate test"}
    end;
init_per_testcase(_Tc, Config) ->
    Config.
end_per_testcase(_Tc, Config) ->
    Config.

c_to_c(Config) ->
    run_benchmark(Config, c, c).
c_to_erlang(Config) ->
    run_benchmark(Config, c, erlang).
erlang_to_c(Config) ->
    run_benchmark(Config, erlang, c).
erlang_to_erlang(Config) ->
    run_benchmark(Config, erlang, erlang).

run_benchmark(Config, From, To) ->
    run_benchmark(Config, From, To,
		  ?config(active, Config),
		  ?config(type, Config),
		  ?config(benchmark_name, Config),
		  ?config(benchmark_opts, Config)).

run_benchmark(Config, ClientType, ServerType, Active, ConnType, Name, Opts) ->
    pong = net_adm:ping(?config(node_tag(local), Config)),
    io:format(
      "run_benchmark ~p ~s -> ~s, ~s, active: ~p\n",
      [Name, ClientType, ServerType, ConnType, Active]),
    ServerHdl = start_server(ServerType, Config, ConnType, Active),
    ServerHost = ?config(host_tag(ConnType), Config),
    Result = run_client(ClientType, Config, [{active,Active},
					     {host,ServerHost} | Opts]),

    stop_server(ServerHdl),

    Suffix =
	case ConnType of
	    local -> "";
	    remote -> "_remote"
	end,
    ActiveSuffix = case Active of
		 once -> "";
		 true -> "_active";
		 false -> "_passive"
	     end,
    Title = lists:flatten(io_lib:format("~s_~s_to_~s~s~s",
					[Name,ClientType,ServerType,
					 ActiveSuffix,Suffix])),
    Stones = result_to_benchmark(Result),
    ct_event:notify(
      #event{name = benchmark_data,
	     data = [{suite,proplists:get_value(suite,Config,?MODULE)},
		     {name,Title},
		     {value,Stones}]}),
    {comment, Title ++ " : " ++ integer_to_list(Stones)}.

result_to_benchmark({micros,Micros}) ->
    ?MICROS_STONE_DIVIDENT div Micros;
result_to_benchmark({throughput,Thrput}) ->
    Thrput div ?THRPUT_STONE_DIVISOR.

run_client(c, Config, Opts) ->
    DataDir = ?config(data_dir,Config),
    ClientExe = case ?config(protocol,Config) of
		    udp -> "udp_client";
		    undefined -> "client"
		end,
    Cmd = filename:join(DataDir, ClientExe),
    Args = client_opts_to_args(Opts),
    Client = spawn_link(?MODULE,exec_start,[self(), Cmd, Args]),
    {0, Result} = c_client_loop(Client, undefined),
    Result;

run_client(erlang, C, Opts) ->
    run_client(gen_tcp, C, Opts);

run_client(TcpMod, _, Opts) ->
    Before = erlang:monotonic_time(),
    netmarks_client:run(Opts, TcpMod),
    Latency = erlang:monotonic_time() - Before,
    {micros, erlang:convert_time_unit(Latency, native, micro_seconds)}.


c_client_loop(Client, Acc) ->
    receive
	{Client, term, {micros,_}=Micros} ->
	    undefined = Acc,
	    c_client_loop(Client, Micros);
	{Client, term, {throughput,_}=Thrput} ->
	    undefined = Acc,
	    c_client_loop(Client, Thrput);
	{Client, term, _} -> % ignore other data from client
	    c_client_loop(Client, Acc);
	{Client, exit_status, Status} ->
	    {Status, Acc};
	Other ->
	    io:format("Unexpected msg: ~p\n", [Other])
    end.


start_server(erlang, Config, ConnType, Active) ->
    ServerNode = ?config(node_tag(ConnType), Config),
    ServerModule = case ?config(protocol,Config) of
		       udp -> netmarks_udp_server;
		       undefined -> netmarks_server
		   end,
    pong = net_adm:ping(ServerNode),
    Server = spawn_link(ServerNode,
			ServerModule, run,
			[[{master,self()},{active,Active}]]),
    receive {Server, ready_to_serve} -> ok end,
    {erlang,ServerNode,Server};

start_server(c, Config, ConnType, _) ->
    ServerNode = ?config(node_tag(ConnType), Config),
    DataDir = ?config(data_dir,Config),
    Cmd = filename:join(DataDir, "server"),
    Server = spawn_link(ServerNode,?MODULE,exec_start,[self(), Cmd, []]),
    io:format("Waiting for ready_to_serve...\n", []),
    ready_to_serve = receive
			 {Server, term, Msg} -> Msg
		     after 10*1000 ->
			     "Timeout waiting for server ready"
		     end,
    io:format("Got ready_to_serve\n", []),
    {c,Server}.

stop_server({erlang,_Node,Pid}) ->
    MRef = monitor(process, Pid),
    unlink(Pid),
    ExitReason = "please die",
    exit(Pid, ExitReason),
    receive {'DOWN', MRef, process, Pid, ActualReason} -> ok end,
    ExitReason = ActualReason,
    ok;

stop_server({c,Proxy}) ->
    Proxy ! {self(), kill_exec},
    0 = receive
	    {Proxy, exit_status, Exit} -> Exit
	after 10*10000 ->
		"Timeout waiting for server exit"
	end,
    ok.


exec_start(Master, Cmd, Args) ->
    io:format("Executing ~p with args ~p\n",[Cmd,Args]),
    Port = erlang:open_port({spawn_executable, Cmd},
			     [{line,100}, exit_status,
			      {args, Args}]),
    exec_loop(Master, Port).


exec_loop(Master, Port) ->
    receive
	{Port, {exit_status, Status}} ->
	    io:format("Got exit status from port program: ~p\n", [Status]),
	    catch erlang:port_close(Port),
	    Master ! {self(), exit_status, Status};
	{Port, {data,{eol, Line}}} ->
	    io:format("Got line (term) from port program: ~p\n", [Line]),
	    Master ! {self(), term, string_to_term(Line)},
	    exec_loop(Master, Port);
	{Master, kill_exec} ->
	    port_command(Port, "."),
	    io:format("Waiting for server to exit\n",[]),
	    exec_loop(Master, Port);
	Msg ->
	    io:format("Unexpected msg while waiting for port program: ~p\n",[Msg]),
	    exec_loop(Master, Port)
    end.

host_tag(local) -> netmarks_server_host;
host_tag(remote) -> netmarks_remote_server_host.

node_tag(local) -> netmarks_server_node;
node_tag(remote) -> netmarks_remote_server_node.

node_name(local) -> netmarks_server;
node_name(remote) -> netmarks_remote_server.


string_to_term(String) ->
    try
	{ok, Tokens, _} = erl_scan:string(String),
	{ok, Term} = erl_parse:parse_term(Tokens),
	Term
    catch
	_:_ ->
	    io:format("Failed to parse string as term:\n~p\n",[String]),
	    exit({"Parse error:",String})
    end.

client_opts_to_args(Opts) ->
    client_opts_to_args(Opts, []).

client_opts_to_args([], Acc) ->
    Acc;
client_opts_to_args([{active,_} | Tail], Acc0) ->
    client_opts_to_args(Tail, Acc0);
client_opts_to_args([{host,Host} | Tail], Acc0) ->
    Acc1 = Acc0 ++ [to_string(Host)], % host name last
    client_opts_to_args(Tail, Acc1);
client_opts_to_args([{Opt,Val} | Tail], Acc0) ->
    Acc1 = [opt_to_arg(Opt), to_string(Val) | Acc0],
    client_opts_to_args(Tail, Acc1).


to_string(V) when is_list(V) -> V;
to_string(V) when is_atom(V) -> atom_to_list(V);
to_string(V) when is_integer(V) -> integer_to_list(V);
to_string(V) when is_float(V) -> float_to_list(V).

opt_to_arg(sync) -> "-s";
opt_to_arg(parallel) -> "-x";
opt_to_arg(iterations) -> "-n";
opt_to_arg(packet_size) -> "-b";
opt_to_arg(header_size) -> "-h".

add_benchmark_opts(Opts, Config) ->
    {benchmark_opts,
     add_opts(Opts, ?config(benchmark_opts,Config))}.

add_opts([], Acc) -> Acc;
add_opts([{Key,Value} | Rest], Acc) ->
    add_opts(Rest, [{Key,Value} | lists:keydelete(Key, 1, Acc)]).
