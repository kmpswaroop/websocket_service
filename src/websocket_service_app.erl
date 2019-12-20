%%%-------------------------------------------------------------------
%% @doc websocket_servce public API
%% @end
%%%-------------------------------------------------------------------

-module(websocket_service_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(routes:get()),
    {ok, _} = cowboy:start_clear(http_listener, 
    				 [{port, 8080}],
    				 #{env => #{dispatch => Dispatch}}
    				),
    websocket_service_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(http_listener).

%% internal functions
