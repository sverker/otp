-module(t).

-export([go/0]).


go() ->
    M = id(<<5:7>>),
    %%M = id(fun a:a),
    spawn('a@elxa44wgyd3',fun() -> M end).


id(X) ->
     X.
