-module(cowboy_rest_handler).

-behaviour(cowboy_rest).

-export([
	 init/2,
	 allowed_methods/2,
	 content_types_accepted/2,
	 push/2
	]).


init(Req, State) ->
    {cowboy_rest, Req, State}.


allowed_methods(Req, State) ->
    Res = [<<"GET">>, <<"POST">>, <<"OPTIONS">>],
    {Res, Req, State}.


content_types_accepted(Req, State) ->
    ContentType = [
		   {<<"application/json">>, push}
		  ],
    {ContentType, Req, State}.


push(Req, State) ->
    {Method, Body} = parse_request(Req),
    case handle_push(Method, Body) of
	ok ->
	    SuccessResponse = jiffy:encode({[{status, ok}]}),
	    Res = cowboy_req:set_resp_body(SuccessResponse, Req),
	    {true, Res, State};
	{error, Message} ->
	    ErrorResponse = jiffy:encode({[
					   {status, error},
					   {reason, Message}
					  ]}),
	    Res = cowboy_req:set_resp_body(ErrorResponse, Req),
	    {false, Res, State}
    end.


parse_request(Req) ->
    Method = cowboy_req:method(Req),
    case cowboy_req:has_body(Req) of
	true -> 
	    {ok, Body_Raw, _} = cowboy_req:read_body(Req),
	    Body_Ejson = jiffy:decode(Body_Raw),
	    {Method, Body_Ejson};
	_ ->
	    {Method, undefined}
    end.


handle_push(<<"POST">>, {[
			  {<<"topic">>, Topic},
			  {<<"data">>, Data}
			 ]}) ->
    topic:publish(Topic, Data),
    ok;
handle_push(_, _) ->
    {error, <<"Invalid Request">>}.
