-module(parallel).
-import(sequential, [euler/1, printElapsed/2]).
-export([sumTotient/2]).

sumTotient(Lower, Upper) ->
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
    receive
        {ready, WorkerPid} ->
            handle_worker_ready(
                Current, Upper, ChunkSize, ActiveWorkers, AccSum, WorkerPid, ParentPid
            );
        {result, _, Sum, _} ->
            io:format("Received result: ~p~n", [Sum]),
            server(Current, Upper, ChunkSize, ActiveWorkers, AccSum + Sum, ParentPid)
    end.

handle_worker_ready(Current, Upper, ChunkSize, ActiveWorkers, AccSum, WorkerPid, ParentPid) ->
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
    WorkerPid ! {work, Current, EndOfChunk},
    server(Current + ChunkSize, Upper, ChunkSize, ActiveWorkers, AccSum, ParentPid).

finish_worker(Current, Upper, ChunkSize, ActiveWorkers, AccSum, WorkerPid, ParentPid) ->
    WorkerPid ! stop,
    case ActiveWorkers - 1 of
        0 ->
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
            Sum = lists:sum([euler(N) || N <- lists:seq(Start, End)]),
            Server ! {result, {Start, End}, Sum, self()},
            Server ! {ready, self()},
            worker_loop(Server);
        stop ->
            ok
    end.
