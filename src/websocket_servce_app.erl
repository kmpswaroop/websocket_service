%%%-------------------------------------------------------------------
%% @doc websocket_servce public API
%% @end
%%%-------------------------------------------------------------------

-module(websocket_servce_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    websocket_servce_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
