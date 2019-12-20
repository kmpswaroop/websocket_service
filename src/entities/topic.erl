-module(topic).

-behaviour(gen_server).

-export([
	 add_subscription/2,
	 remove_subscription/2,
	 publish/2
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
    ets:new(topic, 
	    [set, public, named_table, {write_concurrency, true}]
	   ).


add_subscription(Topic, ClientId) ->
    case ets:lookup(topic, Topic) of
	[{Topic, Clients}] ->
	    ets:insert(topic, {Topic, [ClientId | Clients]});
	[] ->
	    ets:insert(topic, {Topic, [ClientId]})
    end,
    ok.


remove_subscription(Topic, ClientId) ->
    case ets:lookup(topic, Topic) of
	[{Topic, Clients}] ->
	    ets:insert(topic, {Topic, lists:delete(ClientId, Clients)}),
	    ok;
	[] ->
	    {error, topic_not_found}
    end.


publish(Topic, Data) ->
    case ets:lookup(topic, Topic) of
	[{Topic, Clients}] ->
	    lists:foreach(
	      fun(ClientId) ->
		      client:send(ClientId, Data)
	      end,
	      Clients
	     );
	_ ->
	    ok
    end.


handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ets:delete(topic).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
