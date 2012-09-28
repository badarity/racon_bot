-module(racon_bot_app).
-author("Nikita Lebedev <nlebedev@reksoft.ru>").

-behaviour(application).
-export([start_group/2,start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for racon-backend.
start(_Type, _StartArgs) ->
    racon_bot_group_equal:start(450, racon_bot_stupid, ["localhost", 8081]).

start_group(Bots, Games) ->
    GroupStarter = 
        fun(_) -> racon_bot_group_equal:start(Bots, racon_bot_radial,
                                              ["192.168.134.104", 8080])
        end,
    lists:foreach(GroupStarter, lists:seq(1,Games)).


%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for racon-backend.
stop(_State) ->
    ok.
