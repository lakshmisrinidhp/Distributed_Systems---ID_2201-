-module(worker).
-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
    Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
    % Initialize the rand module with the given seed
    rand:seed(exsplus, {Seed, Seed, Seed}),
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, time:zero());
        stop ->
            ok
    end.

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, Clock) ->
    Wait = rand:uniform(Sleep),  % Use rand:uniform/1 instead of random:uniform/1
    receive
        {msg, Time, Msg} ->
            NewClock = time:merge(Clock, Time),
            Log ! {log, Name, NewClock, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, time:inc(Name, NewClock));
        stop ->
            ok;
        Error ->
            Log ! {log, Name, Clock, {error, Error}},
            loop(Name, Log, Peers, Sleep, Jitter, Clock)
    after Wait ->
        Selected = select(Peers),
        Message = {hello, rand:uniform(100)},  % Use rand:uniform/1
        Selected ! {msg, Clock, Message},
        jitter(Jitter),
        Log ! {log, Name, Clock, {sending, Message}},
        loop(Name, Log, Peers, Sleep, Jitter, time:inc(Name, Clock))
    end.

select(Peers) ->
    lists:nth(rand:uniform(length(Peers)), Peers).  % Use rand:uniform/1

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).  % Use rand:uniform/1
