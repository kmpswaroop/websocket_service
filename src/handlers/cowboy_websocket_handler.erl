-module(cowboy_websocket_handler).

-behaviour(cowboy_websocket).

-export([
	 init/2,
	 websocket_init/1,
	 websocket_handle/2,
	 websocket_info/2,
	 terminate/3
	]).
-export([
	 send/2
	]).


init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.


websocket_init(_State) ->
    {[], maps:new()}.


websocket_handle({text, Message}, State) ->
    case handle_event(jiffy:decode(Message)) of
	{ok, ClientId} -> 
	    Out = jiffy:encode({[{status, ok}]}),
	    NewState = maps:put(client_id, ClientId, State),
	    {reply, {text, Out}, NewState};
	{error, ErrMessage} -> 
	    Out = jiffy:encode({[{status, error}, 
				 {message, ErrMessage}
				]}),
	    {reply, {close, Out}, State}
    end;
websocket_handle(_, State) ->
    Out = jiffy:encode({[{status, error}, 
			 {message, <<"Invalid Message Format">>}
		        ]}),
    {reply, {close, Out}, State}.


websocket_info({text, Text}, State) ->
    {reply, {text, Text}, State};
websocket_info(_Info, State) ->
    {ok, State}.


terminate(_Reason, _PartialReq, State) ->
    ClientId = maps:get(client_id, State),
    ok = client:disconnect(ClientId),
    ok.


send(Data, {pid, Pid}) ->
    Pid ! {text, Data},
    ok.


handle_event({[
	       {<<"type">>, <<"connect">>},
	       {<<"version">>, _Version},
	       {<<"client_id">>, ClientId}
	      ]}) ->
    Handler = {cowboy_websocket_handler, send, [{pid, self()}]},
    ok = client:connect(ClientId, Handler),
    {ok, ClientId};
handle_event({[
	       {<<"type">>, <<"subscribe">>},
	       {<<"client_id">>, ClientId},
	       {<<"topic">>, Topic}
	      ]}) ->
    topic:add_subscription(Topic, ClientId),
    {ok, ClientId};
handle_event({[
	       {<<"type">>, <<"unsubscribe">>},
	       {<<"client_id">>, ClientId},
	       {<<"topic">>, Topic}
	      ]}) ->
    topic:remove_subscription(Topic, ClientId),
    {ok, ClientId};
handle_event(_) ->
    {error, <<"Invalid Message">>}.
