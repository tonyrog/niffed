-module(niffed_test).

%% -on_load(init/0).
-export([load_nif/0, load_niffed/0]).
-export([info/0, hello/1, hello/2, goodbye/1]).
-export([echo/1,reverse/1]).

info() ->
    niffed:call(?MODULE, info, []).

hello(X) ->
    niffed:call(?MODULE, hello, [X]).

hello(X,Y) ->
    niffed:call(?MODULE, hello, [X,Y]).

goodbye(L) ->
    niffed:call(?MODULE, goodbye, [L]).

echo(Term) ->
    niffed:call(?MODULE, echo, [Term]).

reverse(List) ->
    niffed:call(?MODULE, reverse, [List]).

load_nif() ->
    Nif = filename:join([code:priv_dir(niffed), ?MODULE]),
    erlang:load_nif(Nif, 0).

load_niffed() ->
    Lib = filename:join([code:priv_dir(niffed), ?MODULE]),
    niffed:load(Lib),
    [niffed:register(?MODULE, Function, Arity) ||
	{Function,Arity} <- module_info(exports), 
	Function =/= load_nif,
	Function =/= load_niffed,
	Function =/= module_info ].
