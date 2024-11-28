-module(parallel).
-import(sequential, [euler/1, printElapsed/2]).
-export([sumTotient/2]).

sumTotient(Lower, Upper) ->
    io:format("Starting with range ~p to ~p~n", [Lower, Upper]),
    ChunkSize = 10,
    NumWorkers = 4,

    Server = spawn(fun() ->
        server(Lower, Upper, ChunkSize, NumWorkers, 0, self())
    end),

    [spawn(fun() -> worker(Server) end) || _ <- lists:seq(1, NumWorkers)],

    % Wait for final result
    receive
        {final_result, FinalSum} ->
            io:format("Sum of totients: ~p~n", [FinalSum]),
            FinalSum
    end.

server(Current, Upper, ChunkSize, ActiveWorkers, AccSum, ParentPid) ->
    io:format(
        "Server state: Current=~p, Upper=~p, Active=~p, Sum=~p~n",
        [Current, Upper, ActiveWorkers, AccSum]
    ),
    receive
        {ready, WorkerPid} ->
            io:format("Worker ~p ready~n", [WorkerPid]),
            handle_worker_ready(
                Current, Upper, ChunkSize, ActiveWorkers, AccSum, WorkerPid, ParentPid
            );
        {result, Range, Sum, WorkerPid} ->
            io:format("Got result ~p from ~p for range ~p~n", [Sum, WorkerPid, Range]),
            handle_worker_ready(
                Current, Upper, ChunkSize, ActiveWorkers, AccSum + Sum, WorkerPid, ParentPid
            )
    end.

handle_worker_ready(Current, Upper, ChunkSize, ActiveWorkers, AccSum, WorkerPid, ParentPid) ->
    io:format("Handling worker ~p ready, Current=~p, Upper=~p~n", [WorkerPid, Current, Upper]),
    case Current =< Upper of
        true ->
            assign_next_chunk(
                Current, Upper, ChunkSize, ActiveWorkers, AccSum, WorkerPid, ParentPid
            );
        false ->
            finish_worker(Current, Upper, ChunkSize, ActiveWorkers, AccSum, WorkerPid, ParentPid)
    end.

assign_next_chunk(Current, Upper, ChunkSize, ActiveWorkers, AccSum, WorkerPid, ParentPid) ->
    EndOfChunk = min(Current + ChunkSize - 1, Upper),
    io:format("Assigning chunk ~p-~p to worker ~p~n", [Current, EndOfChunk, WorkerPid]),
    WorkerPid ! {work, Current, EndOfChunk},
    server(Current + ChunkSize, Upper, ChunkSize, ActiveWorkers, AccSum, ParentPid).

finish_worker(Current, Upper, ChunkSize, ActiveWorkers, AccSum, WorkerPid, ParentPid) ->
    io:format("Finishing worker ~p, Active=~p~n", [WorkerPid, ActiveWorkers]),
    WorkerPid ! stop,
    case ActiveWorkers - 1 of
        0 ->
            io:format("All workers done, sending final result ~p~n", [AccSum]),
            ParentPid ! {final_result, AccSum};
        RemainingWorkers ->
            server(Current, Upper, ChunkSize, RemainingWorkers, AccSum, ParentPid)
    end.

worker(Server) ->
    Server ! {ready, self()},
    worker_loop(Server).

worker_loop(Server) ->
    receive
        {work, Start, End} ->
            io:format("Worker ~p processing range ~p-~p~n", [self(), Start, End]),
            Sum = lists:sum([euler(N) || N <- lists:seq(Start, End)]),
            Server ! {result, {Start, End}, Sum, self()},
            Server ! {ready, self()},
            % ^ this causes race condition
            worker_loop(Server);
        stop ->
            io:format("Worker ~p stopping~n", [self()]),
            ok
    end.
