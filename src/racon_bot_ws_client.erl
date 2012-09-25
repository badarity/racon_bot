-module(racon_bot_ws_client).

-behaviour(websocket_client).
-export([start_link/3, move/2]).
-export([ws_onopen/1, ws_onmessage/1, ws_onclose/0]).

start_link(Host, Port, Path) ->
    websocket_client:start(Host, Port, Path, ?MODULE).

move(Pid, Direction) ->
    Pid ! {move, Direction}.

ws_onopen(Pid) ->
    ok.

ws_onclose() ->
    ok.

ws_onmessage(Data) ->
    ok.




