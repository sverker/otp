-module(sverk).

-export([go/0]).

go() ->
    Bin = <<0, 0, 0, 4, 1, 2, 3>>,
    erts_debug:get_internal_state([heap_consistency_check,break,Bin]),  %% OK
    foo(Bin).    

foo(<<Size:4/unit:8, _B:Size/bytes, _Rest/bytes>>) ->
    ok;
foo(Bin) ->
    erts_debug:get_internal_state([heap_consistency_check,?LINE]), %% CRASH    
    Bin.



