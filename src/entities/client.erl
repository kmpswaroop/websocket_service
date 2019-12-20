-module(client).

-behaviour(gen_server).

-export([
	 connect/2,
	 disconnect/1,
	 send/2
	]).
-export([
	 start_link/0,
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3
	]).

-define(SERVER, ?MODULE).


start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).


init([]) ->
    init_ets(),
    {ok, #{}}.


init_ets() ->
    ets:new(client, 
	    [set, public, named_table, {write_concurrency, true}]
	   ).

-spec connect(
	any(), 
	{module(), function(), [term()]}
       ) -> ok.
connect(ClientId, Handler) ->
    true = ets:insert(client, {ClientId, Handler}),
    ok.


disconnect(ClientId) ->
    true = ets:delete(client, ClientId),
    ok.


send(ClientId, Data) ->
    case ets:lookup(client, ClientId) of
	[{ClientId, {Mod, Fun, Args}}] ->
	    erlang:apply(Mod, Fun, [Data | Args]);
	_ -> 
	    {error, client_not_found}
    end.


handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ets:delete(client).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
