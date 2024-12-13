-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

% Creates an empty set of interfaces
new() -> [].

% Adds a new interface to the set
add(Name, Ref, Pid, Intf) ->
    [{Name, Ref, Pid} | Intf].

% Removes an interface from the set
remove(Name, Intf) ->
    lists:keydelete(Name, 1, Intf).

% Finds the Pid of an interface given its name
lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        {_, _, Pid} -> {ok, Pid};
        false -> notfound
    end.

% Finds the reference of an interface given its name
ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        {_, Ref, _} -> {ok, Ref};
        false -> notfound
    end.

% Finds the name of an interface given its reference
name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
        {Name, _, _} -> {ok, Name};
        false -> notfound
    end.

% Lists all names in the set of interfaces
list(Intf) ->
    [Name || {Name, _, _} <- Intf].

% Broadcasts a message to all interfaces
broadcast(Message, Intf) ->
    lists:foreach(fun({_, _, Pid}) -> Pid ! Message end, Intf).
