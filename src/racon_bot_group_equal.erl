%% run multuiple equal bots

-module(racon_bot_group_equal).

-export([start/3]).

start(Bots, Module, Args) ->
    spawn(fun() -> bot_starer(Bots, Module, Args) end).

bot_starer(Bots, Module, Args) ->
    [ apply(Module,start_link,Args) || _N <- lists:seq(1, Bots) ].
