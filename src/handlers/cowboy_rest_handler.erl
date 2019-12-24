-module(cowboy_rest_handler).

-behaviour(cowboy_rest).

-export([
	 init/2,
	 allowed_methods/2,
	 content_types_accepted/2,
	 publish/2
	]).


-spec init(cowboy_req:req(), any()) ->
		  {atom(), cowboy_req:req(), any()}.
init(Req, State) ->
    {cowboy_rest, Req, State}.


-spec allowed_methods(cowboy_req:req(), any()) -> 
			     {[binary()], cowboy_req:req(), any()}.
allowed_methods(Req, State) ->
    Res = [<<"GET">>, <<"POST">>, <<"OPTIONS">>],
    {Res, Req, State}.


-spec content_types_accepted(cowboy_req:req(), any()) -> 
				    {[{binary(), atom()}], cowboy_req:req(), any()}.
content_types_accepted(Req, State) ->
    ContentType = [
		   {<<"application/json">>, push}
		  ],
    {ContentType, Req, State}.


-spec publish(cowboy_req:req(), any()) -> 
		     {boolean(), cowboy_req:req(), any()}.
publish(Req, State) ->
    {Method, Body} = parse_request(Req),
    case handle_publish(Method, Body) of
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


-spec parse_request(cowboy_req:req()) ->
			   {binary(), jiffy:jiffy_decode_result() | undefined}.
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


-spec handle_publish(binary(), jiffy:json_value()) ->
			    ok |
			    {error, binary()}.
handle_publish(<<"POST">>, {[
			  {<<"topic">>, Topic},
			  {<<"data">>, Data}
			 ]}) ->
    topic:publish(Topic, Data),
    ok;
handle_publish(_, _) ->
    {error, <<"Invalid Request">>}.
