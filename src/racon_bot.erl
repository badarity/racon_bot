%% behaviour module for bot creation
-module(racon_bot).
-behaviour(gen_server).

-export([start_link/3, gamestate/2]).
-export([behaviour_info/1]).
-export([init/1, terminate/2, handle_cast/2, handle_info/2]).

-record(state, {uid = undefined, ws, host, port, gid,%% own
                module, state }). %% behaviour

start_link(Module, Host, Port) ->
    gen_server:start_link(?MODULE, {Module, Host, Port}, []).

gamestate(Pid, Gamestate) ->
    gen_server:cast(Pid, {gamestate, Gamestate}).

behaviour_info(callbacks) ->
    [{init,0},{field_update,2}].

init({Module, Host, Port}) ->
    process_flag(trap_exit, true),
    Gid = get_gid(Host, Port),
    WsConn = ws_connect(Host, Port, Gid, undefined),
    {ok, State} = Module:init(),
    {ok, #state{ws = WsConn, gid = Gid, host = Host, port = Port,
                module = Module, state = State}}.

handle_cast({gamestate, Gamestate}, #state{module = Module} = State) ->
    {noreply, field_update(Module:field_update(Gamestate), State)}.

handle_info({'EXIT', Pid, _Reason},
            #state{ws = Pid, host = Host, port = Port,
                   gid = Gid, uid = Uid} = State) ->
    {noreply, State#state{ws = ws_connect(Host, Port, Gid, Uid)}}.

terminate(_Reason, _State) ->
    ok.

get_gid(Host, Port) ->
    1.

ws_connect(Host, Port, Gid, Uid) ->
    Path = ws_path(Gid, Uid),
    racon_bot_ws_client:start_link(Host, Port, Path).

ws_path(Gid, undefined) ->
    "/game/" ++ integer_to_list(Gid);
ws_path(Gid, Uid) ->
    ws_path(Gid, undefined) ++ "?uid=" ++ integer_to_list(Uid).

field_update({ok, stay, ClientState}, State) ->
    State#state{state = ClientState};

field_update({ok, Direction, ClientState}, State) ->
    move(Direction, State),
    State#state{state = ClientState}.

move(Direction, #state{ws = WsConn}) ->
    racon_bot_ws_client:move(WsConn, Direction).
                                                         

