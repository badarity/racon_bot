%% behaviour module for bot creation
-module(racon_bot).
-behaviour(gen_server).

-export([start/3, start_link/3, gamestate/3, gameinit/3]).
-export([behaviour_info/1]).
-export([init/1, terminate/2, handle_cast/2, handle_info/2]).

-record(state, {uid = undefined, ws, host, port, gid,%% own
                module, state }). %% behaviour

start_link(Module, Host, Port) ->
    gen_server:start_link(?MODULE, {Module, Host, Port, undefined}, []).

start(Module, Host, Port) ->
    gen_server:start(?MODULE, {Module, Host, Port, undefined}, []).


gamestate(Pid, SelfPos, EnemiesPos) ->
    gen_server:cast(Pid, {gamestate, SelfPos, EnemiesPos}).

gameinit(Pid, Gid, Uid) ->
    gen_server:cast(Pid, {gameinit, Gid, Uid}).

behaviour_info(callbacks) ->
    [{init,0},{field_update,3}].

init({Module, Host, Port, Gid}) ->
    process_flag(trap_exit, true),
    WsConn = ws_connect(Host, Port, Gid, undefined),
    {ok, State} = Module:init(),
    {ok, #state{ws = WsConn, host = Host, port = Port,
                module = Module, state = State}}.

handle_cast({gamestate, Self, Enemies}, #state{module = Module} = State) ->
    {noreply, field_update(Module:field_update(Self, Enemies, State), State)};

handle_cast({gameinit, Gid, Uid}, State) ->
    io:format("self:~p~n", [self()]),
    {noreply, State#state{gid = Gid, uid = Uid}}.


handle_info({'EXIT', Pid, _Reason},
            #state{ws = Pid, host = Host, port = Port,
                   gid = Gid, uid = Uid} = State) ->
    {noreply, State#state{ws = ws_connect(Host, Port, Gid, Uid)}}.

terminate(_Reason, _State) ->
    io:format("reason:~p~n~p~n", [_Reason, _State]),
    ok.

ws_connect(Host, Port, Gid, Uid) ->
    Path = ws_path(Gid, Uid),
    {ok, Pid} = racon_bot_ws_client:start_link(Host, Port, Path),
    Pid.

ws_path(undefined, undefined) ->
    "/game";
ws_path(Gid, undefined) ->
    ws_path(undefined, undefined) ++
        "?guid=" ++ Gid;
ws_path(Gid, Uid) ->
    ws_path(Gid, undefined) ++ "uuid=" ++ Uid.

field_update({ok, stay, ClientState}, State) ->
    State#state{state = ClientState};

field_update({ok, Direction, ClientState}, State) ->
    move(Direction, State),
    State#state{state = ClientState}.

move(Direction, #state{ws = WsConn}) ->
    racon_bot_ws_client:move(WsConn, Direction).
                                                         

