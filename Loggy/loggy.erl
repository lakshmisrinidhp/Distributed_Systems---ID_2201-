-module(loggy).
-export([start/1, stop/1]).

start(Nodes) ->
    spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    Clocks = time:clock(Nodes),
    loop(Clocks, []).

loop(Clocks, HoldbackQueue) ->
    receive
        {log, From, Time, Msg} ->
            UpdatedClocks = time:update(From, Time, Clocks),
            UpdatedQueue = [{From, Time, Msg} | HoldbackQueue],
            SafeMessages = deliver_safe(UpdatedQueue, UpdatedClocks),
            loop(UpdatedClocks, SafeMessages);
        stop ->
            ok
    end.

deliver_safe(Queue, Clock) ->
    % Sort the holdback queue by logical time to process in order
    SortedQueue = lists:sort(fun({_, T1, _}, {_, T2, _}) -> T1 =< T2 end, Queue),
    RemainingMessages = lists:foldl(
        fun({From, Time, Msg}, RemainingAcc) ->
            % Only print the message if it's safe (i.e., no earlier messages)
            case time:safe(Time, Clock) of
                true ->
                    % Print the message only after "sending" occurs before "received"
                    case Msg of
                        {received, ReceivedMsg} ->
                            % Check if the corresponding "sending" message exists
                            case lists:any(
                                    fun({_, _, {sending, SentMsg}}) -> 
                                        SentMsg =:= ReceivedMsg;
                                    (_) -> 
                                        false
                                    end, 
                                    Queue) of
                                true ->
                                    io:format("log: ~w ~w ~p~n", [Time, From, Msg]),
                                    RemainingAcc;
                                false ->
                                    [{From, Time, Msg} | RemainingAcc]  % Keep in queue if not safe
                            end;
                        {sending, _} ->
                            io:format("log: ~w ~w ~p~n", [Time, From, Msg]),
                            RemainingAcc;
                        _ ->
                            io:format("log: ~w ~w ~p~n", [Time, From, Msg]),
                            RemainingAcc
                    end;
                false ->
                    [{From, Time, Msg} | RemainingAcc]  % Keep unsafe messages
            end
        end,
        [], SortedQueue),
    lists:reverse(RemainingMessages).  % Return remaining (unsafe) messages



