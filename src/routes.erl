-module(routes).


-export([
	 get/0
	]).


get() ->
    [
     {'_', [
	    {"/websocket", cowboy_websocket_handler, []}
	   ]
     }
    ].
