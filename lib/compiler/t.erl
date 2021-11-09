-module(t).

-export([go/0]).

-nifs([foo/0, bar/2]).

go() ->
    [foo(), bar(1,2)].

foo() ->
   foo.

bar(A,B) ->
    {bar, A, B}.
