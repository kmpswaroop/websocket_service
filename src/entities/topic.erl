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


-spec start_link() ->
			{ok, pid()} |
			{error, {already_started, pid()}} |
			{error, term()} |
			ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).


-spec init(term()) ->
		  {ok, term()}.
init([]) ->
    init_ets(),
    {ok, #{}}.


-spec init_ets() ->
		      atom().
init_ets() ->
    ets:new(topic, 
	    [set, public, named_table, {write_concurrency, true}]
	   ).


-spec add_subscription(binary(), 
		       binary()) -> 
			      ok.
add_subscription(Topic, ClientId) ->
    case ets:lookup(topic, Topic) of
	[{Topic, Clients}] ->
	    ets:insert(topic, {Topic, [ClientId | Clients]});
	[] ->
	    ets:insert(topic, {Topic, [ClientId]})
    end,
    ok.


-spec remove_subscription(binary(), 
			  binary()) ->
				 ok |
				 {error, topic_not_found}.
remove_subscription(Topic, ClientId) ->
    case ets:lookup(topic, Topic) of
	[{Topic, Clients}] ->
	    ets:insert(topic, {Topic, lists:delete(ClientId, Clients)}),
	    ok;
	[] ->
	    {error, topic_not_found}
    end.


-spec publish(binary(), binary()) ->
		     ok.
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
    end,
    ok.


-spec handle_call(term(), {pid(), term()}, term()) ->
			 {reply, ignored, term()}.
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.


-spec handle_cast(term(), term()) ->
			 {noreply, term()}.
handle_cast(_Msg, State) ->
    {noreply, State}.


-spec handle_info(term() | timeout(), term()) ->
			 {noreply, term()}.
handle_info(_Info, State) ->
    {noreply, State}.


-spec terminate(normal | shutdown | {shutdown, term()} | term(),
	        term()) ->
		       any().
terminate(_Reason, _State) ->
    ets:delete(client).


-spec code_change(term() | {down, term()},
		  term(),
		  term()) ->
			 {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
