-module(racon_bot_stupid).
-author("Nikita Lebedev <nlebedev@reksoft.ru>").

-behaviour(racon_bot).
-export([start/2, start_link/2]).
-export([init/0, field_update/3]).

start_link(Host, Port) ->
    racon_bot:start_link(?MODULE, Host, Port).

start(Host, Port) ->
    racon_bot:start(?MODULE, Host, Port).

init() ->
    {ok, {}}.

field_update(Self, Enemies, State) ->
    {ok, down, State}.
    
