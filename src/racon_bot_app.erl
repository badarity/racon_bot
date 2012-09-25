-module(racon_app).
-author("Nikita Lebedev <nlebedev@reksoft.ru>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for racon-backend.
start(_Type, _StartArgs) ->
    racon_bot_group_equal:start(450, racon_bot_stupid, ["localhost", 8081]).

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for racon-backend.
stop(_State) ->
    ok.
