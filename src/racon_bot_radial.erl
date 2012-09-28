-module(racon_bot_radial).
-author("Nikita Lebedev <nlebedev@reksoft.ru>").

-behaviour(racon_bot).

-export([start/3, start/4]).
-export([init/3, field_update/3]).

-record(state, {start_track = [], track = [],
                walked = [], prev_pos = undefined}).

-include("priv/include/delays.hrl").
-compile([export_all]).

start(Host, Port, Mod) ->
    racon_bot:start(?MODULE, Host, Port, {Mod, self()}).

start(Host, Port, Gid, Mod) ->
    racon_bot:start(?MODULE, Host, Port, Gid, {Mod, self()}).

init(Gid, Uid, {GroupMod, Group}) ->
    GroupMod:connected(Group, Gid),
    {ok, #state{}}.

field_update(dead, _Enemies, State) ->
    {stop, State};
%% initial position
field_update(Pos, _Enemies, #state{track = [], walked = []} = State) ->
    FieldSize = {30,30},
    {StartTrack, RadialTrack} = create_track(Pos, FieldSize),
    step(State#state{start_track = StartTrack,
                     track = RadialTrack, prev_pos = Pos});
field_update(Pos, _Enemies, #state{prev_pos = Pos} = State) ->
    {ok, stay, State};
field_update(Pos, _Enemies, State) ->
    step(State#state{prev_pos = Pos}).

step(#state{start_track = [Move | Rest]} = S) ->
    {ok, Move, S#state{start_track = Rest}};
step(#state{track = [], walked = Walked}) ->
    step(#state{track = lists:reverse(Walked), walked = []});
step(#state{track = [Move | NextMoves], walked = Walked} = S) ->
    move_delay(),
    {ok, Move, S#state{track = NextMoves, walked = [Move | Walked]}}.

%% create_track(start_pos, field_size)
create_track(Start, FieldSize) ->
    Rad = choose_radius(FieldSize),
    {track_to_circle(Start, Rad, FieldSize), [up,left,down,right]}.

move_delay() ->
    receive nothing -> ok after ?MOVE_DELAY -> ok end.

choose_radius({H, W}) ->
    MinRad = 2,
    randim:uniform(lists:min([H div 2,W div 2]) - MinRad + 1) + MinRad - 1.

track_to_circle(Start, Rad, FieldSize) ->
    track_to_point(Start, point_on_circle(Rad, FieldSize)).

point_on_circle(Rad, FieldSize) ->
    Points = circle_points(Rad, FieldSize),
    lists:nth(random:uniform(length(Points)), Points).

circle_points(Rad, {W, H}) ->
    {Cx, Cy}= Center = {W div 2, H div 2},
    cycle_track({Cx, Cy - Rad}, Rad, Center).

cycle_track(Start, Rad, Center) ->
    cycle_track(Start, Start, [], Rad, Center).

cycle_track(Start, {Px, Py} = Prev, Track, Rad, Center) ->
    D = [-1,0,1],
    Moves = clockwise_moves([ {Dx, Dy} || Dx <- D, Dy <- D, abs(Dx + Dy) == 1 ], Prev, Center),
    Ps = [ {Mx+Px, My+Py} || {Mx, My} <- Moves ],
    add_track_point(Start, precisest_point(Ps, Rad, Center), Track, Rad, Center).

add_track_point(Start, Start, Track, _Rad, _Center) ->
    lists:reverse(Track);
add_track_point(Start, Point, Track, Rad, Center) ->
    cycle_track(Start, Point, [Point | Track], Rad, Center).


clockwise_moves(Moves, {Sx,Sy}, {Cx, Cy}) ->
    [ {Mx, My} || {Mx, My} <- Moves, Mx * (Sy - Cy) >= 0, My * (Sx - Cx) =< 0].

precisest_point(Points, RealRad, {Cx, Cy}) ->
    Rad = fun({X, Y}) -> (X - Cx) * (X - Cx) + (Y - Cy) * (Y - Cy) end,
    Folder =
	fun({Measure, P}, {Min, MinP}) ->
	   case mabs(Measure) < mabs(Min) of
	       true -> {Measure, P};
	       _ -> {Min, MinP}
	   end
	end,
    {_, PP} = lists:foldl(Folder, {false,undefined},
			  [ { RealRad*RealRad - Rad(P), P} || P <- Points ]),
    io:format("~p~n", [PP]),
    PP.

mabs(false) ->
    false;
mabs(Number) ->
    abs(Number).

track_to_point(From, To) ->
    [up,down].
    

