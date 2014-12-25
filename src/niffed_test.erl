-module(niffed_test).

%% -on_load(init/0).
-export([init/0]).
-export([hello/1, hello/2, goodbye/1]).

hello(X) ->
    niffed:call(?MODULE, hello, [X]).

hello(X,Y) ->
    niffed:call(?MODULE, hello, [X,Y]).

goodbye(L) ->
    niffed:call(?MODULE, goodbye, [L]).

init() ->
    Lib = filename:join([code:priv_dir(niffed), ?MODULE]),
    niffed:load(Lib),
    niffed:register(?MODULE, hello, 1),
    niffed:register(?MODULE, hello, 2),
    niffed:register(?MODULE, goodbye, 1).
