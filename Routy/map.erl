-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

% Creates an empty map
new() -> [].

% Updates the map with the new links for a node
update(Node, Links, Map) ->
    Map1 = lists:keydelete(Node, 1, Map),
    [{Node, Links} | Map1].

% Returns the list of nodes reachable from Node
reachable(Node, Map) ->
    case lists:keyfind(Node, 1, Map) of
        {_, Links} -> Links;
        false -> []
    end.

% Returns all the nodes in the map
all_nodes(Map) ->
    lists:foldl(fun({Node, Links}, Acc) ->
        lists:usort([Node | Links ++ Acc])
    end, [], Map).
