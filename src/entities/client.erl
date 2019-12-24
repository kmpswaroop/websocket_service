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
    ets:new(client, 
	    [set, public, named_table, {write_concurrency, true}]
	   ).


-spec connect(binary(), {module(), function(), [term()]}) -> 
		     ok.
connect(ClientId, Handler) ->
    true = ets:insert(client, {ClientId, Handler}),
    ok.


-spec disconnect(binary()) ->
			ok.
disconnect(ClientId) ->
    true = ets:delete(client, ClientId),
    ok.


-spec send(binary(),iodata()) ->
		  ok |
		  {error, atom()}.
send(ClientId, Data) ->
    case ets:lookup(client, ClientId) of
	[{ClientId, {Mod, Fun, Args}}] ->
	    erlang:apply(Mod, Fun, [Data | Args]);
	_ -> 
	    {error, client_not_found}
    end.


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
