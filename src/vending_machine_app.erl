%%%-------------------------------------------------------------------
%% @doc vending_machine public API
%% @end
%%%-------------------------------------------------------------------

-module(vending_machine_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    vending_machine_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
