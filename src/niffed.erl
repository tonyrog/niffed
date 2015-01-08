%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2014, Tony Rogvall
%%% @doc
%%%    niffed server 
%%% @end
%%% Created : 24 Dec 2014 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(niffed).

-behaviour(gen_server).

%% API
-export([start/0]).
-export([start_link/0]).
-export([load/1, register/3, call/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, niffed_data).
-record(state,
	{
	  niffed_data     %% ets: atom() -> port()
	                  %% ets: {atom(),atom(),integer()} -> integer()
	}).

-define(CTL_LOOKUP_NIF,  0).
-define(CTL_LOOKUP_ATOM, 1).
-define(CTL_CALL,        2).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(niffed).

load(Lib) ->
    gen_server:call(?SERVER, {load,Lib}).

register(M,F,A) ->
    gen_server:call(?SERVER, {register,{M,F,A}}).

call(Mod, Func, Args) ->
    Arity = length(Args),
    case ets:lookup(?TABLE, Mod) of
	[{_,Port}] ->
	    case ets:lookup(?TABLE, {Mod,Func,Arity}) of
		[{_,Index}] ->
		    Lookup = fun(A) when is_atom(A) -> encode_atom(Port,A) end,
		    NifData = niffed_codec:encode_call(Lookup, Index, Args),
		    case erlang:port_control(Port, ?CTL_CALL, NifData) of
			<<0,_/binary>> ->
			    exit(badarg);
			<<1,Data/binary>> ->
			    io:format("reply = ~p\n", [Data]),
			    niffed_codec:decode(Data)
		    end;
		_ -> exit(undef)
	    end;
	_ ->  exit(undef)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ets:new(niffed_data, [protected, named_table]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({load,Lib}, _From, State) ->
    case load_driver(Lib) of
	{ok,Module} ->
	    Port = erlang:open_port({spawn_driver,Module},[binary]),
	    ets:insert(?TABLE, {Module, Port}),
	    {reply, ok, State};
	Error ->
	    {reply, Error, State}
    end;
handle_call({register,{M,F,A}}, _From, State) ->
    case ets:lookup(?TABLE, M) of
	[{_,Port}] ->
	    Name = atom_to_binary(F, latin1),
	    Arg = <<(byte_size(Name)):8,Name/binary,A:8>>,
	    case erlang:port_control(Port, ?CTL_LOOKUP_NIF, Arg) of
		<<0,_/binary>> -> {reply, {error, badarg}, State};
		<<1,Bytes/binary>> ->
		    Ws = bit_size(Bytes),
		    <<Index:Ws/native>> = Bytes,
		    ets:insert(?TABLE, {{M,F,A}, Index}),
		    {reply, ok, State}
	    end;
	[] ->
	    {reply, {error, enoent}, State}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

encode_atom(Port, A) ->
    case erlang:port_control(Port, ?CTL_LOOKUP_ATOM, atom_to_list(A)) of
	<<0,_/binary>> -> exit(badarg);
	<<1,Bytes/binary>> ->
	    Ws = bit_size(Bytes),
	    <<Cell:Ws/native>> = Bytes,
	    Cell
    end.


%% can be replaced with dloader later
load_driver(DriverPath) ->
    Ext = filename:extension(DriverPath),
    Name = filename:basename(DriverPath,Ext),
    Path = filename:dirname(DriverPath),
    case erl_ddll:load(Path, Name) of
	ok ->
	    {ok, list_to_atom(Name)};
	Error ->
	    Error
    end.
