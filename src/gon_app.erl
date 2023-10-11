%%%-------------------------------------------------------------------
%% @doc gon public API
%% @end
%%%-------------------------------------------------------------------

-module(gon_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    gon_sup:start_link(application:get_env(gon, viewsize, 10),
                       application:get_env(gon, pswap_pss, 0.5),
                       application:get_env(gon, pswap_app, 0.5)).

stop(_State) ->
    ok.

%% internal functions
