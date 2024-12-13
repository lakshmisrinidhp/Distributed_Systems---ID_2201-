-module(routy).
-export([start/2, stop/1, broadcast/1, update/1, route/3, status/1]).

% Starts a new router process
start(Reg, Name) ->
    register(Reg, spawn(fun() -> init(Name) end)).

% Stops a router process
stop(Node) ->
    Node ! stop,
    unregister(Node).

% Initializes a router with empty values
init(Name) ->
    Intf = intf:new(),
    Map = map:new(),
    Table = dijkstra:table([], Map),
    Hist = hist:new(Name),
    router(Name, 0, Hist, Intf, Table, Map).

% Main router loop
router(Name, N, Hist, Intf, Table, Map) ->
    receive
        {add, Node, Pid} ->
            Ref = erlang:monitor(process, Pid),
            Intf1 = intf:add(Node, Ref, Pid, Intf),
            io:format("Added interface ~p to ~p~n", [Node, Name]),
            router(Name, N, Hist, Intf1, Table, Map);

        {remove, Node} ->
            {ok, Ref} = intf:ref(Node, Intf),
            erlang:demonitor(Ref),
            Intf1 = intf:remove(Node, Intf),
            io:format("Removed interface ~p from ~p~n", [Node, Name]),
            router(Name, N, Hist, Intf1, Table, Map);

        {links, Node, R, Links} ->
            case hist:update(Node, R, Hist) of
                {new, Hist1} ->
                    io:format("Received new link-state message from ~p~n", [Node]),
                    intf:broadcast({links, Node, R, Links}, Intf),
                    Map1 = map:update(Node, Links, Map),
                    router(Name, N, Hist1, Intf, Table, Map1);
                old ->
                    io:format("Received old link-state message from ~p~n", [Node]),
                    router(Name, N, Hist, Intf, Table, Map)
            end;

        broadcast ->
            Message = {links, Name, N, intf:list(Intf)},
            io:format("Broadcasting link-state from ~p: ~p~n", [Name, Message]),
            intf:broadcast(Message, Intf),
            router(Name, N + 1, Hist, Intf, Table, Map);

        update ->
            Table1 = dijkstra:table(intf:list(Intf), Map),
            io:format("Updating routing table for ~p: ~p~n", [Name, Table1]),
            router(Name, N, Hist, Intf, Table1, Map);

        {route, To, From, Message} ->
            case dijkstra:route(To, Table) of
                {ok, Gw} ->
                    case intf:lookup(Gw, Intf) of
                        {ok, Pid} ->
                            io:format("Forwarding message from ~p to ~p via ~p~n", [From, To, Gw]),
                            Pid ! {route, To, From, Message};
                        notfound -> io:format("Gateway not found for ~p~n", [Gw])
                    end;
                notfound -> io:format("No route found for ~p~n", [To])
            end,
            router(Name, N, Hist, Intf, Table, Map);

        {status, From} ->
            From ! {status, {Name, N, Hist, Intf, Table, Map}},
            router(Name, N, Hist, Intf, Table, Map);

        stop -> ok
    end.

% Broadcast link-state message
broadcast(Node) ->
    case whereis(Node) of
        undefined ->
            io:format("Router ~p is not running~n", [Node]);
        _Pid ->
            Node ! broadcast
    end.

% Manually update the routing table for a router
update(Node) ->
    case whereis(Node) of
        undefined ->
            io:format("Router ~p is not running~n", [Node]);
        _Pid ->
            Node ! update
    end.

% Routes a message from one node to another
route(Name, To, Message) ->
    Name ! {route, To, Name, Message}.

% Requests and prints the current status of a router
status(Name) ->
    Name ! {status, self()},
    receive
        {status, State} -> io:format("Status: ~p~n", [State])
    end.
