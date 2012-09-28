%% behaviour module for bot creation
-module(racon_bot).
-behaviour(gen_server).

-export([start/4, start/5, start_link/4, start_link/5, gamestate/3, gameinit/3]).
-export([behaviour_info/1]).
-export([init/1, terminate/2, handle_cast/2, handle_info/2]).

-record(state, {uid = undefined, ws, host, port, gid,%% own
                module, state, initialized = false, args }). %% behaviour

start_link(Module, Host, Port, Args) ->
    gen_server:start_link(?MODULE, {Module, Host, Port, undefined, Args}, []).

start_link(Module, Host, Port, Gid, Args) ->
    gen_server:start_link(?MODULE, {Module, Host, Port, Gid, Args}, []).

start(Module, Host, Port, Args) ->
    gen_server:start(?MODULE, {Module, Host, Port, undefined, Args}, []).

start(Module, Host, Port, Gid, Args) ->
    gen_server:start(?MODULE, {Module, Host, Port, Gid, Args}, []).


gamestate(Pid, SelfPos, EnemiesPos) ->
    gen_server:cast(Pid, {gamestate, SelfPos, EnemiesPos}).

gameinit(Pid, Gid, Uid) ->
    gen_server:cast(Pid, {gameinit, Gid, Uid}).

behaviour_info(callbacks) ->
    [{init,3},{field_update,3}].

init({Module, Host, Port, Gid, Args}) ->
    process_flag(trap_exit, true),
    WsConn = ws_connect(Host, Port, Gid, undefined),
    {ok, #state{ws = WsConn, host = Host, port = Port,
                module = Module, args = Args}}.

handle_cast({gamestate, Self, Enemies}, #state{module = Module,
                                               state = CState} = State) ->
    field_update(Module:field_update(Self, Enemies, CState), State);

handle_cast({gameinit, Gid, Uid}, State) ->
    {noreply, callback_init(State#state{gid = Gid, uid = Uid})}.

handle_info({'EXIT', Pid, _Reason},
            #state{ws = Pid, host = Host, port = Port,
                   gid = Gid, uid = Uid} = State) ->
    {noreply, State#state{ws = ws_connect(Host, Port, Gid, Uid)}}.

callback_init(#state{initialized = true} = State) ->
    State;
callback_init(#state{gid = Gid, uid = Uid, module = Module, args = Args} = State) ->
    {ok, CState} = Module:init(Gid, Uid, Args),
    State#state{state = CState, initialized = true}.

terminate(Reason, #state{ws = WsConn}) ->
    racon_bot_ws_client:stop(WsConn),
    ok.

ws_connect(Host, Port, Gid, Uid) ->
    Path = ws_path(Gid, Uid),
    {ok, Pid} = racon_bot_ws_client:start_link(Host, Port, Path),
    Pid.

ws_path(undefined, undefined) ->
    "/game";
ws_path(Gid, undefined) ->
    ws_path(undefined, undefined) ++
        "?GUID=" ++ http_uri:encode(Gid);
ws_path(Gid, Uid) ->
    ws_path(Gid, undefined) ++ "&uuid=" ++ http_uri:encode(Uid).

field_update({stop, ClientState}, State) ->
    {stop, normal, State#state{state = ClientState}};

field_update({ok, stay, ClientState}, State) ->
    {noreply, State#state{state = ClientState}};

field_update({ok, Direction, ClientState}, State) ->
    move(Direction, State),
    {noreply, State#state{state = ClientState}}.

move(Direction, #state{ws = WsConn}) ->
    racon_bot_ws_client:move(WsConn, Direction).
