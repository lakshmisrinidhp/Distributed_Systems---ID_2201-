-module(time).
-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

% Initialize the Lamport clock
zero() -> 0.

% Increment the Lamport time by 1
inc(_Name, T) -> T + 1.

% Merge two Lamport clocks (take the max)
merge(Ti, Tj) -> max(Ti, Tj).

% Compare if Ti is less than or equal to Tj
leq(Ti, Tj) -> Ti =< Tj.

% Initialize a clock for all nodes
clock(Nodes) ->
    [{Node, 0} || Node <- Nodes].

% Update clock with a message from a node
update(Node, Time, Clock) ->
    lists:keyreplace(Node, 1, Clock, {Node, merge(Time, get_time(Node, Clock))}).

get_time(Node, Clock) ->
    case lists:keyfind(Node, 1, Clock) of
        {Node, Time} -> Time;
        false -> 0
    end.

% Check if a time is safe to log
safe(Time, Clock) ->
    % Safe if all nodes have logged up to the given time
    lists:all(fun({_, NodeTime}) -> Time =< NodeTime end, Clock).
