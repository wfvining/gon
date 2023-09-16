%%%-------------------------------------------------------------------
%% @doc gon public API
%% @end
%%%-------------------------------------------------------------------

-module(gon_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gon_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
