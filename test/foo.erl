-module(foo).

-export([start_nodes/1, run_test/1]).

run_test(N) ->
    Nodes = start_nodes(N),
    go(Nodes).

start_nodes(0) ->
    [];
start_nodes(N) ->
    [start_node() | start_nodes(N - 1)].

start_node() ->
    NodeName = list_to_atom(peer:random_name()),
    {ok, NodeRef, Node} = peer:start_link(
                            #{name => NodeName,
                              args => ["-pa", hd(code:get_path())],
                              connection => standard_io}),
    {NodeRef, Node}.

go(Nodes) ->
    %% 1. Start the gon application on each node
    lists:foreach(
      fun ({NodeRef, _}) -> ok = peer:call(NodeRef, application, start, [gon]) end,
      Nodes),
    %% 2. select initial peers at random for each node
    NodeNames = [Node || {_, Node} <- Nodes],
    lists:foreach(
      fun ({NodeRef, _}) -> set_peers(NodeRef, NodeNames) end,
      Nodes),
    timer:send_interval(500, tick),
    loop(Nodes, 600000).

set_peers(NodeRef, Nodes) ->
    Sample = select(10, Nodes),
    peer:cast(NodeRef, pss, peer, [Sample]).

select(N, Nodes) ->
    select(N, Nodes, []).

select(0, _, Sample) ->
    Sample;
select(N, Nodes, Sample) ->
    X = rand:uniform(length(Nodes)),
    Node = lists:nth(X, Nodes),
    select(N - 1, Nodes -- [Node], [Node | Sample]).

loop(Nodes, TimeRemaining) ->
    loop(Nodes, TimeRemaining, []).

loop(Nodes, TimeRemaining, History) when TimeRemaining =< 0 ->
    P = node_positions(Nodes),
    H = lists:flatten(lists:reverse(History)),
    {ok, CSV} = file:open("pos.csv", [write]),
    io:format(CSV, "node,x,y~n", []),
    lists:foreach(fun({N, {X, Y}}) -> io:format(CSV, "~p,~p,~p~n", [N, X, Y]) end, P),
    file:close(CSV),
    {ok, DAT} = file:open("adjacency.dat", [write]),
    History1 = [{600000 - T, N, A} || {T, N, A} <- H],
    lists:foreach(
      fun({T, N, A}) ->
              io:format(DAT, "~p ~s ~s~n",
                        [T, atom_to_list(N),
                         string:join([atom_to_list(Neighbor) || Neighbor <- A], ",")])
      end, History1),
    file:close(DAT);
loop(Nodes, TimeRemaining, History) ->
    %% 3. periodically poll the structured overlay layer for neighbors
    %% 4. save the adjacency list after polling all nodes
    receive
        tick ->
            Peers = poll_nodes(Nodes, TimeRemaining),
            loop(Nodes, TimeRemaining - 500, [Peers|History])
    end.

node_positions(Nodes) ->
    [{NodeName, peer:call(Node, overlay, position, [])} || {Node, NodeName} <- Nodes].

poll_nodes(Nodes, Time) ->
    [{Time, NodeName, peer:call(Node, overlay, peers, [], 30000)} || {Node, NodeName} <- Nodes].
