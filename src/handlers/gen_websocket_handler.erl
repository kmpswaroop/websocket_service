-module(gen_websocket_handler).

-callback send(iodata()) -> ok.
