-module(upgrade_client).

-export([run/5]).

-define(line, io:format("~s:~p\n", [?MODULE,?LINE]),).
    

run(Dir, Upgradee1, Upgradee2, Other1, Other2) ->
    %% Load version 1 of upgradee
    code_SUITE:compile_load(upgradee, Dir, 1, Upgradee1),

    ?line 1 = upgradee:exp1(),
    ?line 1 = upgradee:exp1exp2(),
    ?line 1 = upgradee:exp1loc2(),

    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1exp2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1loc2()),

    P1 = spawn_link(upgradee,dispatch_loop,[]),

    ?line 1 = proxy_call(P1, local, exp1),
    ?line 1 = proxy_call(P1, local, loc1),
    ?line 1 = proxy_call(P1, local, exp1exp2),
    ?line 1 = proxy_call(P1, local, exp1loc2),
    ?line 1 = proxy_call(P1, local, loc1exp2),
    ?line 1 = proxy_call(P1, local, loc1loc2),
    ?line 1 = proxy_call(P1, external, exp1),
    ?line 1 = proxy_call(P1, external, exp1exp2),
    ?line 1 = proxy_call(P1, external, exp1loc2),

    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, loc1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, loc2),
    ?line {cannot_compile,1} = proxy_call(P1, local, exp2),
    ?line {cannot_compile,1} = proxy_call(P1, local, loc2),

    ?line {'EXIT',{undef,_}} = (catch other:exp1()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1()),
    ?line {'EXIT',{undef,_}} = (catch other:exp1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:exp1exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc11exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc2()),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, exp1),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, exp1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, exp1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc2),

    %%
    %% Load version 1 of other
    %%
    code_SUITE:compile_load(other, Dir, 1, Other1),
    ?line 1 = other:exp1(),
    ?line 1 = other:exp1loc2(),
    ?line 1 = other:exp1exp2(),
    ?line {'EXIT',{undef,_}} = (catch other:loc1()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc2()),

    ?line 1 = proxy_call(P1, other, exp1),
    ?line 1 = proxy_call(P1, other, exp1loc2),
    ?line 1 = proxy_call(P1, other, exp1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc2),

    %%
    %% Load version 2 of upgradee
    %%
    code_SUITE:compile_load(upgradee, Dir, 2, Upgradee2),

    ?line 2 = upgradee:exp2(),
    ?line 2 = upgradee:exp1exp2(),
    ?line 2 = upgradee:loc1exp2(),
    
    erlang:display(sverk_break),
    %%?line
    {'EXIT',{undef,_}} = (catch upgradee:exp1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1loc2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc2()),

    ?line 1 = proxy_call(P1, local, exp1),
    ?line 1 = proxy_call(P1, local, loc1),
    ?line 1 = proxy_call(P1, local, exp1exp2),
    ?line 1 = proxy_call(P1, local, exp1loc2),
    ?line 1 = proxy_call(P1, local, loc1exp2),
    ?line 1 = proxy_call(P1, local, loc1loc2),
    ?line {cannot_compile,1} = proxy_call(P1, local, exp2),
    ?line {cannot_compile,1} = proxy_call(P1, local, loc2),

    ?line 2 = proxy_call(P1, external, exp1exp2),
    ?line 2 = proxy_call(P1, external, loc1exp2),
    ?line 2 = proxy_call(P1, external, exp2),

    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, exp1),    
    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, exp1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, loc2),

    ?line 1 = other:exp1(),
    ?line 1 = other:exp1loc2(),
    ?line 1 = other:exp1exp2(),
    ?line {'EXIT',{undef,_}} = (catch other:loc1()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:exp2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc2()),

    ?line 1 = proxy_call(P1, other, exp1),
    ?line 1 = proxy_call(P1, other, exp1loc2),
    ?line 1 = proxy_call(P1, other, exp1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc2),

    %%
    %% Load version 2 of other
    %%
    code_SUITE:compile_load(other, Dir, 2, Other2),

    ?line 2 = upgradee:exp2(),
    ?line 2 = upgradee:exp1exp2(),
    ?line 2 = upgradee:loc1exp2(),
    
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1loc2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:loc2()),

    ?line 1 = proxy_call(P1, local, exp1),
    ?line 1 = proxy_call(P1, local, loc1),
    ?line 1 = proxy_call(P1, local, exp1exp2),
    ?line 1 = proxy_call(P1, local, exp1loc2),
    ?line 1 = proxy_call(P1, local, loc1exp2),
    ?line 1 = proxy_call(P1, local, loc1loc2),
    ?line {cannot_compile,1} = proxy_call(P1, local, exp2),
    ?line {cannot_compile,1} = proxy_call(P1, local, loc2),

    ?line 2 = proxy_call(P1, external, exp1exp2),
    ?line 2 = proxy_call(P1, external, loc1exp2),
    ?line 2 = proxy_call(P1, external, exp2),

    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, exp1),    
    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, exp1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, external, loc2),

    ?line 2 = other:exp2(),
    ?line 2 = other:loc1exp2(),
    ?line 2 = other:exp1exp2(),
    ?line {'EXIT',{undef,_}} = (catch other:exp1()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1()),
    ?line {'EXIT',{undef,_}} = (catch other:exp1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc1loc2()),
    ?line {'EXIT',{undef,_}} = (catch other:loc2()),

    ?line 2 = proxy_call(P1, other, exp2),
    ?line 2 = proxy_call(P1, other, loc1exp2),
    ?line 2 = proxy_call(P1, other, exp1exp2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, exp1),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc1),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, exp1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc1loc2),
    ?line {'EXIT',{undef,_}} = proxy_call(P1, other, loc2),

    unlink(P1),
    exit(P1, die_please),

    io:format("Delete and purge 'upgradee'\n",[]),
    code:purge(upgradee),
    code:delete(upgradee),
    code:purge(upgradee),

    io:format("Delete and purge 'other'\n",[]),
    code:purge(other),
    code:delete(other),
    code:purge(other),
    ok.

proxy_call(Pid, CallType, Func) ->
    Pid ! {self(), CallType, Func},
    receive
	{Pid, call_result, Func, Ret} -> Ret
    end.
