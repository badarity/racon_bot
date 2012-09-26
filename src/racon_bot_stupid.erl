-module(racon_bot_stupid).
-author("Nikita Lebedev <nlebedev@reksoft.ru>").

-behaviour(racon_bot).
-export([start/3, start/4, start_link/3]).
-export([init/3, field_update/3]).

start_link(Host, Port, Mod) ->
    racon_bot:start_link(?MODULE, Host, Port, {Mod, self()}).

start(Host, Port, Mod) ->
    racon_bot:start(?MODULE, Host, Port, {Mod, self()}).

start(Host, Port, Gid, Mod) ->
    racon_bot:start(?MODULE, Host, Port, Gid, {Mod, self()}).

init(Gid, Uid, {GroupMod, Group}) ->
    GroupMod:connected(Group, Gid),
    {ok, {down, {0,0}}}.

field_update(dead, Enemies, State) ->
    {stop, State};
field_update({X, Y}, _Enemies, {_D, {X, Y}} = State) ->
    {ok, stay, State};
field_update(Pos, _Enemies, {Direction, _PPos}) ->
    NewDirection = step(Pos, Direction),
    {ok, NewDirection, {NewDirection, Pos}}.

step({0, 0}, _D) ->
    move(up);
step({15, 0}, down) ->
    move(left);
step({29, 29}, _D) ->
    move(down);
step({29, 0}, _D) ->
    move(left);
step({0, 29}, _D) ->
    move(right);
step(_Pos, Direction) ->
    move(Direction).


move(Direction) ->
    receive nothing -> ok after 150 -> ok end,
    Direction.
