-module(racon_bot_radial).
-author("Nikita Lebedev <nlebedev@reksoft.ru>").

-behaviour(racon_bot).

-export([start_link/3, start_link/4]).
-export([init/3, field_update/3]).

-record(state, {start_track = [], track = [],
                walked = [], prev_pos = undefined,
                timer, leader}).
-record(timer, {last_check, last_diff}).

-include("priv/include/delays.hrl").

start_link(Host, Port, Mod) ->
    racon_bot:start_link(?MODULE, Host, Port, {Mod, self()}).

start_link(Host, Port, Gid, Mod) ->
    racon_bot:start_link(?MODULE, Host, Port, Gid, {Mod, self()}).

init(Gid, Uid, {GroupMod, Group}) ->
    GroupMod:connected(Group, Gid),
    {ok, #state{leader = Group, timer = #timer{}}}.

field_update(dead, _Enemies, #state{leader = Leader} = State) ->
    racon_bot_group_equal:moved(Leader),
    {stop, State};
%% initial position
field_update(Pos, _Enemies, #state{track = [], walked = []} = State) ->
    FieldSize = {30,30},
    {StartTrack, RadialTrack} = create_track(Pos, FieldSize),
    step(State#state{start_track = StartTrack,
                     track = RadialTrack, prev_pos = Pos});
field_update(Pos, _Enemies, #state{prev_pos = Pos} = State) ->
    {ok, stay, State};
field_update(Pos, _Enemies, #state{leader = Leader} = State) ->
    step(State#state{prev_pos = Pos}).

step(#state{start_track = [Move | Rest]} = S) ->
    {ok, Move, S#state{start_track = Rest}};
step(#state{track = [], walked = Walked} = State) ->
    step(State#state{track = lists:reverse(Walked), walked = []});
step(#state{track = [Move | NextMoves], walked = Walked} = S) ->
    TimerStop = move_timer_stop(S),
    move_delay(),
    TimerStart = move_timer_start(TimerStop),
    {ok, Move, TimerStart#state{track = NextMoves, walked = [Move | Walked]}}.

%% create_track(start_pos, field_size)
create_track(Start, FieldSize) ->
    Rad = choose_radius(FieldSize),
    {TrackToCircle, [CStart | Circle]} = cycle_track(Start, Rad, field_center(FieldSize)),
    {track_to_moves(Start, TrackToCircle ++ [CStart]),
     track_to_moves(CStart, Circle ++ [CStart]) }.

field_center({W, H}) ->
    {W div 2, H div 2}.

track_to_moves(Start, Track) ->
    track_to_moves(Start, Track, []).

track_to_moves(_Prev, [], Moves) ->
    lists:reverse(Moves);
track_to_moves(Prev, [Next | Track], Moves) ->
    track_to_moves(Next, Track, [pair_to_move(Prev, Next) | Moves]).

pair_to_move({Fx, Fy}, {Tx, Ty}) ->
    diff_to_move(Tx - Fx, Ty - Fy).

diff_to_move(-1, 0) ->
    left;
diff_to_move(1, 0) ->
    right;
diff_to_move(0, -1) ->
    down;
diff_to_move(0, 1) ->
    up.

move_delay() ->
    receive nothing -> ok after ?MOVE_DELAY -> ok end.

choose_radius({H, W}) ->
    MinRad = 2,
    random:seed(now()),
    random:uniform(lists:min([H div 2,W div 2]) - MinRad) + MinRad - 1.

cycle_track(Start, Rad, Center) ->
    cycle_track(Start, [], Rad, Center).

cycle_track({Px, Py} = Prev, Track, Rad, Center) ->
    D = [-1,0,1],
    Steps = [ {Dx, Dy} || Dx <- D, Dy <- D, abs(Dx + Dy) == 1 ],
    Moves = clockwise_moves(Steps, Prev, Center),
    Ps = [ {Mx+Px, My+Py} || {Mx, My} <- Moves ],
    add_track_point(precisest_point(Ps, Rad, Center), Track, Rad, Center).

add_track_point(Point, Track, Rad, Center) ->
    split_or_walk(lists:member(Point, Track), Point, Track, Rad, Center).

split_or_walk(true, Point, Track, _Rad, _Center) ->
    {_Tail, _Circle} = lists:splitwith(fun(TP) -> TP =/= Point end, lists:reverse(Track));
split_or_walk(false, Point, Track, Rad, Center) ->
    cycle_track(Point, [Point | Track], Rad, Center).


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
    PP.

mabs(false) ->
    false;
mabs(Number) ->
    abs(Number).

move_timer_stop(#state{timer = #timer{last_check = undefined}} = State) ->
    racon_bot_group_equal:moved(State#state.leader), %% hack
    State;
move_timer_stop(#state{timer = #timer{last_check = Last}} = State) ->
    Now = now(),
    Diff = timer:now_diff(Now, Last),
    report_if_enormous(Diff, State#state.leader),
    State#state{timer = #timer{last_check = Now, last_diff = Diff}}.

move_timer_start(#state{timer = Timer} = State) ->
    State#state{timer = Timer#timer{last_check = now()}}.

report_if_enormous(N, Leader) when N > 100 * 1000 ->
    io:format("the lag is ~p microseconds~n", [N]);
report_if_enormous(_N, _Leader) ->
    ok.
