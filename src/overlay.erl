-module(overlay).

-behaviour(gen_server).

-export([start_link/2, peers/0, position/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SERVER, ?MODULE).

-record(state, {viewsize = 10 :: pos_integer(),
                pinitiate = 0.5 :: float(),
                tref :: timer:tref(),
                position ::  {float(), float()},
                peers = [] :: [{node(), {float(), float()}}],
                queue = queue:new() :: queue:queue(node())}).

start_link(ViewSize, PInitiate) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {ViewSize, PInitiate}, []).

peers() ->
    gen_server:call(?SERVER, peers, infinity).

position() ->
    gen_server:call(?SERVER, position).

init({ViewSize, PInitiate}) ->
    {ok, TRef} = timer:send_interval(1000, tick),
    X = rand:uniform(),
    Y = rand:uniform(),
    {ok, #state{viewsize = ViewSize,
                pinitiate = PInitiate,
                tref = TRef,
                position = {X, Y}}}.

handle_call(position, _From, State) ->
    {reply, State#state.position, State};
handle_call(peers, _From, State) ->
    {reply, [Peer || {Peer, _} <- State#state.peers], State}.

handle_cast({reply, Peers}, State) ->
    NewView = select_to_keep(Peers, State#state.peers, State#state.viewsize,
                             State#state.position),
    {noreply, State#state{peers = NewView}};
handle_cast({offer, From, Peers}, State) ->
    Send = select_to_send(State#state.peers, State#state.viewsize div 2),
    send_reply(From, [{node(), State#state.position} | Send]),
    NewView =
        select_to_keep(Peers, State#state.peers, State#state.viewsize,
                       State#state.position),
    {noreply, State#state{peers = NewView}}.

handle_info(tick, State) ->
    %% Sample a node from the random layer
    %% Put that node into the queue
    %% take the head of the queue to exchange with
    %% get nodes to keep and add them to the queue
    %% remove discarded nodes from the queue
    case pss:select_peer() of
        empty ->
            {noreply, State};
        Node ->
            Q = queue:in(Node, State#state.queue),
            P = rand:uniform(),
            if P < State#state.pinitiate ->
                    {{value, Peer}, NewQ} = queue:out(Q),
                    Send = select_to_send(State#state.peers, State#state.viewsize div 2),
                    send_offer(Peer, [{node(), State#state.position}|Send]),
                    {noreply, State#state{queue = NewQ}};
               true ->
                    {noreply, State#state{queue = Q}}
            end
    end.

send_offer(Dest, Peers) ->
    rpc:call(Dest, gen_server, cast, [?SERVER, {offer, self(), Peers}]).

send_reply(Dest, Peers) ->
    gen_server:cast(Dest, {reply, Peers}).

select_to_keep(Peers, View, ViewSize, {X, Y}) ->
    SortedView =
        lists:sort(fun({_, {X1, Y1}}, {_, {X2, Y2}}) ->
                           D1 = math:sqrt((X1 - X) * (X1 - X) + (Y1 - Y) * (Y1 - Y)),
                           D2 = math:sqrt((X2 - X) * (X2 - X) + (Y2 - Y) * (Y2 - Y)),
                           D1 =< D2
                   end,
                   lists:uniq(fun({N, _}) -> N end, Peers ++ View)),
    lists:sublist(lists:filter(fun({N, _}) -> N =/= node() end, SortedView), 1, ViewSize).

select_to_send(PartialView, N) ->
    select(PartialView, N, []).

select(Xs, N, Selected) when N =:= 0; Xs =:= [] ->
    Selected;
select(Xs, N, Selected) ->
    X = lists:nth(rand:uniform(length(Xs)), Xs),
    select(Xs -- [X], N - 1, [X|Selected]).
