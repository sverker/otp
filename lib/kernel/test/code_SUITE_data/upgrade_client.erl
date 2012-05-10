-module(upgrade_client).

-export([run/3]).

-define(line, io:format("~s:~p\n", [?MODULE,?LINE]),).
    

run(Src, Type1, Type2) ->
    %% Load version 1 of module
    code_SUITE:compile_load(upgradee, Src, 1, Type1),

    {1,1} = upgradee:exp1(),
    1     = upgradee:exp1exp2(),
    1     = upgradee:exp1loc2(),

    {'EXIT',{undef,_}} = (catch upgradee:exp2()),
    {'EXIT',{undef,_}} = (catch upgradee:loc1exp2()),
    
    %% Load version 2 of module
    code_SUITE:compile_load(upgradee, Src, 2, Type2),

    ?line {2,2} = upgradee:exp2(),
    ?line 2     = upgradee:exp1exp2(),
    ?line 2     = upgradee:loc1exp2(),
    
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1()),
    ?line {'EXIT',{undef,_}} = (catch upgradee:exp1loc2()),

    code:purge(upgradee),
    code:delete(upgradee),
    code:purge(upgradee),
    ok.
