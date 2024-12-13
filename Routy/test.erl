-module(test).
-export([run/0, setup/0, test_routing/0, test_failure/0, stop_routers/0]).

% Helper function to stop/unregister any existing routers
stop_routers() ->
    io:format("Stopping any existing routers~n"),
    
    % Check if r1 is running and stop it if it is
    case whereis(r1) of
        undefined -> ok;
        _ -> routy:stop(r1), io:format("Stopped r1~n")
    end,

    % Check if r2 is running and stop it if it is
    case whereis(r2) of
        undefined -> ok;
        _ -> routy:stop(r2), io:format("Stopped r2~n")
    end,

    % Check if r3 is running and stop it if it is
    case whereis(r3) of
        undefined -> ok;
        _ -> routy:stop(r3), io:format("Stopped r3~n")
    end,

    % Check if r4 is running and stop it if it is
    case whereis(r4) of
        undefined -> ok;
        _ -> routy:stop(r4), io:format("Stopped r4~n")
    end,

    % Check if r5 is running and stop it if it is
    case whereis(r5) of
        undefined -> ok;
        _ -> routy:stop(r5), io:format("Stopped r5~n")
    end.

% Starts and sets up the routers with connections
setup() ->
    % Ensure any existing routers are stopped
    stop_routers(),

    % Start routers in different cities
    routy:start(r1, stockholm),
    io:format("Started r1 (stockholm)~n"),
    routy:start(r2, gothenburg),
    io:format("Started r2 (gothenburg)~n"),
    routy:start(r3, malmo),
    io:format("Started r3 (malmo)~n"),
    routy:start(r4, uppsala),
    io:format("Started r4 (uppsala)~n"),
    routy:start(r5, lund),
    io:format("Started r5 (lund)~n"),

    % Manually add interfaces between routers using the router process names (r1, r2, etc.)
    r1 ! {add, gothenburg, {r2, 'sweden@130.123.112.23'}},
    r2 ! {add, stockholm, {r1, 'sweden@130.123.112.23'}},
    r2 ! {add, malmo, {r3, 'sweden@130.123.112.23'}},
    r3 ! {add, gothenburg, {r2, 'sweden@130.123.112.23'}},
    r3 ! {add, uppsala, {r4, 'sweden@130.123.112.23'}},
    r4 ! {add, lund, {r5, 'sweden@130.123.112.23'}},
    r5 ! {add, uppsala, {r4, 'sweden@130.123.112.23'}}.

% Test routing of messages between routers
test_routing() ->
    % Broadcast link-state messages to all interfaces
    routy:broadcast(r1),
    routy:broadcast(r2),
    routy:broadcast(r3),
    routy:broadcast(r4),
    routy:broadcast(r5),

    % Manually update routing tables for all routers after broadcasting
    io:format("Updating routing tables after broadcasting~n"),
    routy:update(r1),
    routy:update(r2),
    routy:update(r3),
    routy:update(r4),
    routy:update(r5),

    % Test routing of messages
    io:format("Testing routing of messages~n"),
    routy:route(r1, lund, "Hello from Stockholm to Lund!"),
    routy:route(r5, stockholm, "Hello from Lund to Stockholm!"),
    routy:route(r2, malmo, "Message from Gothenburg to Malmo!").

% Test failure of a router and recomputation of routes
test_failure() ->
    % Simulate shutting down Uppsala
    io:format("Shutting down Uppsala router~n"),
    routy:stop(r4),

    % Broadcast new link-state messages after Uppsala shutdown
    io:format("Broadcasting link-state updates after Uppsala shutdown~n"),
    routy:broadcast(r1),
    routy:broadcast(r2),
    routy:broadcast(r3),
    routy:broadcast(r5),

    % Recompute routing tables after the failure
    io:format("Recomputing routing tables after Uppsala shutdown~n"),
    routy:update(r1),
    routy:update(r2),
    routy:update(r3),
    routy:update(r5),

    % Test routing after Uppsala shutdown
    io:format("Testing routing after Uppsala failure~n"),
    routy:route(r1, lund, "Message after Uppsala shutdown"),
    routy:route(r5, gothenburg, "Message from Lund to Gothenburg after failure").

% Main function to run the entire test
run() ->
    setup(),
    test_routing(),
    test_failure().
