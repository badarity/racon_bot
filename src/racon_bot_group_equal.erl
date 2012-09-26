%% run multuiple equal bots

-module(racon_bot_group_equal).

-export([start/3, connected/2]).

-define(STARTUP_TIMEOUT, 400).

start(Bots, Module, Args) ->
    spawn(fun() -> bot_starer(Bots, Module, Args) end).

connected(Pid, Gid) ->
    Pid ! {self(), connected, Gid}.

bot_starer(Bots, Module, Args) ->
    start_bot(Module, Args),
    receive
        {Pid, connected, Gid} ->
            [ start_bot(Module, Args ++ [Gid]) || _N <- lists:seq(1, Bots - 1) ]
    end.

    
start_bot(Module, Args) ->
    receive nothing -> ok after ?STARTUP_TIMEOUT -> ok end,
    apply(Module,start,Args ++ [?MODULE]).
