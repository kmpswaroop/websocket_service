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


-spec init(cowboy_req:req(), any()) ->
		  {atom(), cowboy_req:req(), any()}.
init(Req, State) ->
    {cowboy_websocket, Req, State}.


-spec websocket_init(any()) ->
			    {ok, map()}.
websocket_init(_State) ->
    {ok, maps:new()}.


-spec websocket_handle({text, binary()}, any()) ->
			      {reply, {text | close, iodata()}, any()}.
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


-spec websocket_info({text, iodata()}, any()) ->
			    {reply, {text, iodata()}, any()} |
			    {ok, any()}.
websocket_info({text, Text}, State) ->
    {reply, {text, Text}, State};
websocket_info(_Info, State) ->
    {ok, State}.


-spec terminate(atom(), map(), any()) ->
		       ok.
terminate(_Reason, _PartialReq, State) ->
    ClientId = maps:get(client_id, State),
    ok = client:disconnect(ClientId),
    ok.


-spec send(iodata(), {pid, pid()}) ->
		  ok.
send(Data, {pid, Pid}) ->
    Pid ! {text, Data},
    ok.


-spec handle_event(jiffy:json_value()) ->
			  {ok, binary()} |
			  {error, binary()}.
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
    io:format(Topic),
    io:format(ClientId),
    topic:remove_subscription(Topic, ClientId),
    {ok, ClientId};
handle_event(_) ->
    {error, <<"Invalid Message">>}.
