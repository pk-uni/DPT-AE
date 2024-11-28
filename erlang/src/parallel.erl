-module(parallel).
-import(sequential, [euler/1]).
-export([start/3]).

start(Lower, Upper, NumWorkers) ->
    ParentPid = self(),

    ServerState = #{
        current => Lower,
        upper => Upper,
        results => [],
        active_workers => NumWorkers,
        parent => ParentPid
    },

    ServerPid = spawn(fun() -> server(ServerState) end),
    Workers = [spawn(fun() -> worker(ServerPid) end) || _ <- lists:seq(1, NumWorkers)],

    sumTotient(Lower, Upper, Workers).

sumTotient(Lower, Upper, Workers) ->
    receive
        {done, Results} ->
            [WorkerPid ! {stop, self()} || WorkerPid <- Workers],
            wait_workers(length(Workers)),
            io:format("Sum of totients from ~p to ~p is ~p~n", [Lower, Upper, lists:sum(Results)]),
            ok
    end.

wait_workers(0) ->
    ok;
wait_workers(N) ->
    receive
        worker_stopped -> wait_workers(N - 1)
    end.

server(
    #{
        current := Current,
        upper := Upper,
        results := Results,
        active_workers := ActiveWorkers,
        parent := ParentPid
    } = State
) ->
    receive
        {ready, WorkerPid} ->
            WorkerPid ! {work, Current},
            server(State#{current := Current + 1});
        {result, Count, WorkerPid} ->
            case Current > Upper of
                true ->
                    case ActiveWorkers of
                        1 ->
                            ParentPid ! {done, [Count | Results]};
                        _ ->
                            server(State#{
                                results := [Count | Results], active_workers := ActiveWorkers - 1
                            })
                    end;
                false ->
                    WorkerPid ! {work, Current},
                    server(State#{current := Current + 1, results := [Count | Results]})
            end
    end.

worker(ServerPid) ->
    ServerPid ! {ready, self()},
    worker_loop(ServerPid).

worker_loop(ServerPid) ->
    receive
        {work, N} ->
            Count = sequential:euler(N),
            ServerPid ! {result, Count, self()},
            worker_loop(ServerPid);
        {stop, AccumulatorPid} ->
            AccumulatorPid ! worker_stopped,
            ok
    end.
