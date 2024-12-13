-module(hist).
-export([new/1, update/3]).

% Creates a new history for a given node
new(_Name) -> [].

% Updates the history with a new message
update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        {_, Prev} when N =< Prev -> old;
        {Node, _} -> {new, lists:keyreplace(Node, 1, History, {Node, N})};
        false -> {new, [{Node, N} | History]}
    end.
