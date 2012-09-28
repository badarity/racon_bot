%% run multuiple equal bots

-module(racon_bot_group_equal).
-behaviour(gen_server).

-export([start/3, connected/2, moved/1]).
-export([init/1, handle_info/2, terminate/2]).

-include("priv/include/delays.hrl").

-record(state, {gid, active = 0, args, mod}).

start(Bots, Module, Args) ->
    gen_server:start(?MODULE, {Bots, Module, Args}, []).

init({Bots, Module, Args}) ->
    process_flag(trap_exit, true),
    {ok, bot_starer(Bots, Module, Args)}.

connected(Pid, Gid) ->
    Pid ! {self(), connected, Gid}.

moved(Pid) ->
    Pid ! {self(), moved}.

handle_info({_Pid, connected, Gid}, #state{gid = Gid, active = Active} = S) ->
    {noreply, S#state{active = Active + 1}};

%% ingore it
handle_info({_Pid, moved}, State) ->
    {noreply, State};

handle_info({'EXIT', _Pid, _Reason}, #state{gid = Gid, mod = Mod, args = Args} = S) ->
    start_bot(Mod,Args, Gid),
    {noreply, S}.

terminate(_Reason, _State) ->
    ok.

bot_starer(Bots, Module, Args) ->
    Gid = get_gid(Module, Args),
    [ start_bot(Module, Args ++ [Gid]) || _N <- lists:seq(1, Bots - 1) ],
    #state{gid = Gid, active = 1, args = Args, mod = Module}.


get_gid(Module, Args) ->
    start_bot(Module, Args),
    receive
        {_Pid, connected, Gid} ->
            Gid
    end.

start_bot(Module, Args, Gid) ->
    start_bot(Module, Args ++ [Gid]).

start_bot(Module, Args) ->
    {ok, Pid} = apply(Module,start_link,Args ++ [?MODULE]),
    receive
        {Pid, moved} ->
            Pid
    end.
