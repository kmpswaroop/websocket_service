-module(routes).


-export([
	 get/0
	]).


get() ->
    [
     {'_', [
	    {"/websocket/v1/connect", cowboy_websocket_handler, []},
	    {"/api/v1/publish", cowboy_rest_handler, []}
	   ]
     }
    ].
