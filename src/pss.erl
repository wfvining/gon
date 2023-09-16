%%% @doc a simple peer sampling service for peer-to-peer & mesh networks.
%%% @end
%%%
%%% @copyright 2023 Will Vining
%%% @author Will Vining
-module(pss).

-behaviour(gen_server).

-export([start_link/0, select_peer/0, peer/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {partial_view = [] :: [{pid(), age()}],
                time = 0 :: age()}).

-define(SERVER, ?MODULE).
-define(C, 10).
-define(timeout, 1000 + rand:uniform(2000)).

-type age() :: pos_integer().

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Establish a peering relationship between this node and the
%% nodes in `Nodes'
-spec peer(Peers :: [pid()]) -> ok.
peer(Peers) ->
    gen_server:cast(?SERVER, {peer, Peers}).

init([]) ->
    {ok, #state{}}.

select_peer() ->
    gen_server:call(?SERVER, select_peer).

handle_cast({peer, Nodes}, #state{time = Time} = State) ->
    NewPartialView =
        select_to_keep(Time, Nodes, State#state.partial_view, ?C),
    {noreply, State#state{time = Time + 1, partial_view = NewPartialView}, ?timeout};
handle_cast({offer, From, Peers},
            #state{time = Time, partial_view = PartialView} = State) ->
    Send = select_to_send(PartialView, ?C div 2),
    send_reply(From, Send),
    NewPartialView =
        select_to_keep(Time, Peers, State#state.partial_view, ?C),
    {noreply, State#state{time = Time + 1, partial_view = NewPartialView}, ?timeout};
handle_cast({reply, Peers},
            #state{time = Time, partial_view = PartialView} = State) ->
    NewPartialView = select_to_keep(Time, Peers, PartialView, ?C),
    {noreply, State#state{partial_view = NewPartialView}, ?timeout}.

handle_call(select_peer, _From, State) ->
    Peer = select_peer(State#state.partial_view),
    {reply, Peer, State, ?timeout}.

handle_info(timeout, State) ->
    %% Trigger the process to initiate an exchange with a peer
    Send = select_to_send(State#state.partial_view, ?C div 2),
    Peer = select_peer(State#state.partial_view),
    send_offer(Peer, [self()|Send]),
    {noreply, State, ?timeout}.

send_offer(Dest, Peers) ->
    gen_server:cast(Dest, {offer, self(), Peers}).

send_reply(Dest, Peers) ->
    gen_server:cast(Dest, {reply, Peers}).

select_peer(Peers) ->
    N = rand:uniform(length(Peers)),
    element(1, lists:nth(N, Peers)).

select_to_keep(T, NewPeers, PartialView, ViewSize) ->
    Add = [{Pid, T} || Pid <- NewPeers,
                       not(lists:keymember(Pid, 1, PartialView)),
                       Pid =/= self()],
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
