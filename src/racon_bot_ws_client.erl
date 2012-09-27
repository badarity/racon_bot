-module(racon_bot_ws_client).

-behaviour(websocket_client).
-export([start_link/3, move/2, stop/1]).
-export([init/1, ws_onopen/1, ws_onmessage/2, ws_oninfo/2, ws_onclose/1]).

start_link(Host, Port, Path) ->
    websocket_client:start(Host, Port, Path, ?MODULE, self()).

stop(Pid) ->
    websocket_client:close(Pid).

move(Pid, Direction) ->
    Pid ! {move, Direction}.

init(BotPid) ->
    {ok, BotPid}.

ws_onopen(State) ->
    {ok, State}.

ws_onclose(State) ->
    {ok, State}.

ws_oninfo({move, Direction}, State) ->
    {reply, atom_to_list(Direction), State}.

ws_onmessage(Msg, State) ->
    handle_msg(parse_msg(Msg), State).

parse_msg(Msg) ->
    Decoded = mochijson2:decode(Msg),
    parse_by_type(msg_type(Decoded), Decoded).

msg_type({struct, Props}) ->
    Guid = proplists:get_value(<<"guid">>, Props),
    Self = proplists:get_value(<<"self">>, Props),
    case {Guid, Self} of
        {undefined, undefined} ->
            undefined;
        {Guid, undefined} ->
            initial;
        {undefined, Self} ->
            gamestate
    end.

parse_by_type(initial, {struct, Props}) ->
    Gid = proplists:get_value(<<"guid">>, Props),
    Uid = proplists:get_value(<<"uuid">>, Props),
    {initial, binary_to_list(Gid), binary_to_list(Uid)};

parse_by_type(gamestate, {struct, Props}) ->
    Self = parse_self_state(proplists:get_value(<<"self">>, Props)),
    Enemies = proplists:get_value(<<"enemy">>, Props),
    {gamestate, Self, [ {EX, EY} || [EX, EY] <- Enemies ]}.

parse_self_state(<<"dead">>) ->
    dead;
parse_self_state([X, Y]) ->
    {X, Y}.

handle_msg({initial, Gid, Uid}, BotPid) ->
    racon_bot:gameinit(BotPid, Gid, Uid),
    {ok, BotPid};

handle_msg({gamestate, Self, Enemies}, BotPid) ->
    racon_bot:gamestate(BotPid, Self, Enemies),
    {ok, BotPid}.
