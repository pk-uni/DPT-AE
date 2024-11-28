-module(parallel).
-export([start/0]).

start() ->
    NumWorkers = 4,
    Lower = 1,
    Upper = 115,
    BlockSize = 10,
    ParentPid = self(),
    ServerPid = spawn(fun() -> server(Lower, Upper, BlockSize, ParentPid) end),

    [spawn(fun() -> worker(ServerPid) end) || _ <- lists:seq(1, NumWorkers)],

    receive
        {done} -> ok
    end.

server(Current, Upper, BlockSize, ParentPid) ->
    receive
        {ready, WorkerPid} ->
            Start = Current,
            End = min(Current + BlockSize - 1, Upper),
            case End =:= Upper of
                true ->
                    ParentPid ! {done};
                false ->
                    WorkerPid ! {print, {Start, End}},
                    server(Current + BlockSize, Upper, BlockSize, ParentPid)
            end
    end.

worker(ServerPid) ->
    io:format("Worker ~p is up!~n", [self()]),
    ServerPid ! {ready, self()},
    worker_loop(ServerPid).

worker_loop(ServerPid) ->
    receive
        {print, {Start, End}} ->
            handle_print(Start, End),
            ServerPid ! {ready, self()},
            worker_loop(ServerPid)
    end.

handle_print(Start, End) ->
    [io:format("~p~n", [N]) || N <- lists:seq(Start, End)].
