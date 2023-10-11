%%%-------------------------------------------------------------------
%% @doc gon top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(gon_sup).

-behaviour(supervisor).

-export([start_link/3]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(ViewSize, PSwapPSS, PSwapApp) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, {ViewSize, PSwapPSS, PSwapApp}).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init({ViewSize, PSwapPSS, PSwapApp}) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => pss,
                    start => {pss, start_link, [ViewSize, PSwapPSS]},
                    restart => permanent,
                    type => worker,
                    shutdown => 2000}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
