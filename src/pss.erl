%%% @doc a simple peer sampling service for peer-to-peer & mesh networks.
%%% @end
%%%
%%% @copyright 2023 Will Vining
%%% @author Will Vining
-module(pss).

-behaviour(gen_server).

-export([start_link/2, select_peer/0, peer/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {partial_view = [] :: [{pid(), age()}],
                time = 0 :: age(),
                viewsize = 10 :: pos_integer(),
                pinitiate = 0.5 :: float(),
                tref :: timer:tref()}).

-define(SERVER, ?MODULE).

-type age() :: pos_integer().

start_link(ViewSize, PInitiate) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {ViewSize, PInitiate}, []).

%% @doc Establish a peering relationship between this node and the
%% nodes in `Nodes'
-spec peer(Peers :: [pid()]) -> ok.
peer(Peers) ->
    gen_server:cast(?SERVER, {peer, Peers}).

init({ViewSize, PInitiate}) ->
    {ok, TRef} = timer:send_interval(1000, tick),
    {ok, #state{viewsize = ViewSize, pinitiate = PInitiate, tref = TRef}}.

select_peer() ->
    gen_server:call(?SERVER, select_peer).

handle_cast({peer, Nodes}, #state{time = Time} = State) ->
    NewPartialView =
        select_to_keep(Time, Nodes, State#state.partial_view, State#state.viewsize),
    {noreply, State#state{time = Time + 1, partial_view = NewPartialView}};
handle_cast({offer, From, _Peers}, #state{partial_view = []} = State) ->
    gen_server:cast(From, rejected),
    {noreply, State};
handle_cast({offer, From, Peers},
            #state{time = Time, partial_view = PartialView} = State) ->
    Send = select_to_send(PartialView, State#state.viewsize div 2),
    send_reply(From, Send),
    NewPartialView =
        select_to_keep(Time, Peers, State#state.partial_view, State#state.viewsize),
    {noreply, State#state{time = Time + 1, partial_view = NewPartialView}};
handle_cast({reply, Peers},
            #state{time = Time, partial_view = PartialView} = State) ->
    NewPartialView = select_to_keep(Time, Peers, PartialView, State#state.viewsize),
    {noreply, State#state{partial_view = NewPartialView}}.

handle_call(select_peer, _From, #state{partial_view = []} = State) ->
    {reply, empty, State};
handle_call(select_peer, _From, State) ->
    Peer = select_peer(State#state.partial_view),
    {reply, Peer, State}.

handle_info(tick, #state{partial_view = []} = State) ->
    %% Don't do anything if there are no nodes in the partial view.
    {noreply, State};
handle_info(tick, State) ->
    %% Trigger the process to initiate an exchange with a peer
    P = rand:uniform(),
    if P < State#state.pinitiate ->
            Send = select_to_send(State#state.partial_view, State#state.viewsize div 2),
            Peer = select_peer(State#state.partial_view),
            send_offer(Peer, [node()|Send]);
       true ->
            ok
    end,
    {noreply, State}.

send_offer(Dest, Peers) ->
    rpc:call(Dest, gen_server, cast, [?SERVER, {offer, self(), Peers}]).

send_reply(Dest, Peers) ->
    gen_server:cast(Dest, {reply, Peers}).

select_peer(Peers) ->
    N = rand:uniform(length(Peers)),
    element(1, lists:nth(N, Peers)).

select_to_keep(T, NewPeers, PartialView, ViewSize) ->
    Add = [{Peer, T} || Peer <- NewPeers,
                       not(lists:keymember(Peer, 1, PartialView)),
                       Peer =/= node()],
    View = lists:keysort(2, Add ++ PartialView),
    if length(View) =< ViewSize ->
            View;
       length(View) > ViewSize ->
            lists:nthtail(length(View) - ViewSize, View)
    end.

select_to_send(PartialView, N) ->
    select(PartialView, N, []).

select(Xs, N, Selected) when N =:= 0; Xs =:= [] ->
    Selected;
select(Xs, N, Selected) ->
    {Pid, _} = X = lists:nth(rand:uniform(length(Xs)), Xs),
    select(Xs -- [X], N - 1, [Pid|Selected]).
