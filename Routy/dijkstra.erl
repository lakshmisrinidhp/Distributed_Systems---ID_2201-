-module(dijkstra).
-export([table/2, route/2, update/4]).

% Dijkstra's algorithm to compute routing table
table(Gateways, Map) ->
    Nodes = map:all_nodes(Map),
    InitialSorted = [{G, 0, G} || G <- Gateways] ++ [{N, inf, unknown} || N <- Nodes, not lists:member(N, Gateways)],
    iterate(InitialSorted, Map, []).

% Finds the next gateway to route a message
route(Node, Table) ->
    case lists:keyfind(Node, 1, Table) of
        {Node, _, Gateway} when Gateway =:= Node -> 
            io:format("Routing to self: ~p~n", [Node]),
            {ok, Node}; % Handle self-routing: return the node itself
        {Node, _, Gateway} -> 
            io:format("Routing to ~p via gateway ~p~n", [Node, Gateway]),
            {ok, Gateway}; % Handle valid gateway
        false -> 
            io:format("No route found for ~p~n", [Node]),
            notfound % Handle missing route
    end.

% Iteratively calculates the routing table
iterate([], _Map, Table) -> Table;
iterate([{_, inf, _} | _], _Map, Table) -> Table; % Updated: replaced Node with _
iterate([{Node, Dist, Gateway} | Rest], Map, Table) ->
    ReachableNodes = map:reachable(Node, Map),
    UpdatedRest = lists:foldl(
        fun(Reachable, Acc) -> update(Reachable, Dist + 1, Gateway, Acc) end,
        Rest, ReachableNodes),
    iterate(UpdatedRest, Map, [{Node, Gateway} | Table]).

% Updates the sorted list with a shorter path if possible
update(Node, N, Gateway, Sorted) ->
    case entry(Node, Sorted) of
        0 -> Sorted;
        Prev when N < Prev -> replace(Node, N, Gateway, Sorted);
        _ -> Sorted
    end.

% Retrieves the distance to a node from the sorted list
entry(Node, Sorted) ->
    case lists:keyfind(Node, 1, Sorted) of
        {_, D, _} -> D;
        false -> inf
    end.

% Replaces a node's entry with a new one in the sorted list
replace(Node, N, Gateway, Sorted) ->
    Sorted1 = lists:keydelete(Node, 1, Sorted),
    lists:sort([{Node, N, Gateway} | Sorted1]).
