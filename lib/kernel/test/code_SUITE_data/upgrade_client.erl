-module(upgrade_client).

-export([run/3]).

-define(line, io:format("~s:~p\n", [?MODULE,?LINE]),).
    

run(Dir, Type1, Type2) ->
    %% Load version 1 of module
    code_SUITE:compile_load(upgradee, Dir, 1, Type1),

    ?line {1,1} = upgradee:exp1(),
    ?line 1     = upgradee:exp1exp2(),
    ?line 1     = upgradee:exp1loc2(),

    ?line {'EXIT',{undef,_}} = (catch upgradee:exp2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1exp2()),

    P1 = spawn_link(upgradee,dispatch_loop,[]),

    ?line {1,1} = proxy_call(P1, local, exp1),
    ?line 1     = proxy_call(P1, local, exp1exp2),
    ?line 1     = proxy_call(P1, local, exp1loc2),
    ?line 1     = proxy_call(P1, local, loc1exp2),

    ?line {1,1} = proxy_call(P1, external, exp1),
    ?line 1     = proxy_call(P1, external, exp1exp2),
    ?line 1     = proxy_call(P1, external, exp1loc2),

    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, loc1exp2),    
    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, exp2),
    ?line {cannot_compile,1} = proxy_call(P1, local, exp2),

    %%
    %% Load version 2 of upgradee
    %%
    code_SUITE:compile_load(upgradee, Dir, 2, Type2),

    ?line {2,2} = upgradee:exp2(),
    ?line 2     = upgradee:exp1exp2(),
    ?line 2     = upgradee:loc1exp2(),
    
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1loc2()),


    ?line {1,1} = proxy_call(P1, local, exp1),
    ?line 1     = proxy_call(P1, local, exp1exp2),
    ?line 1     = proxy_call(P1, local, exp1loc2),
    ?line 1     = proxy_call(P1, local, loc1exp2),

    ?line 2     = proxy_call(P1, external, exp1exp2),
    ?line 2     = proxy_call(P1, external, loc1exp2),
    ?line {2,2} = proxy_call(P1, external, exp2),

    %?line {'EXIT',{undef,_}} = proxy_call(P1, external, exp1),    
    %?line {'EXIT',{undef,_}} = proxy_call(P1, external, exp1loc2),
    ?line {cannot_compile,1} = proxy_call(P1, local, exp2),

    unlink(P1),
    exit(P1, die_please),

    code:purge(upgradee),
    code:delete(upgradee),
    code:purge(upgradee),
    ok.

proxy_call(Pid, CallType, Func) ->
    Pid ! {self(), CallType, Func},
    receive
	{Pid, call_result, Func, Ret} -> Ret
    end.
