-module(parallel).
-import(sequential, [euler/1, printElapsed/2]).
-import(chaos, [workerName/1]).
-export([start/3]).

start(Lower, Upper, NumWorkers) ->
    spawn(fun() ->
        io:format("Starting program with PID ~p~n", [self()]),
        sumTotient(Lower, Upper, NumWorkers)
    end).

sumTotient(Lower, Upper, NumWorkers) ->
    {_, S, US} = os:timestamp(),

    ServerState = initialize_server_state(Lower, Upper, NumWorkers),

    ServerPid = spawn(fun() -> server(ServerState) end),
    Workers = [spawn_and_register_worker(ServerPid, N) || N <- lists:seq(1, NumWorkers)],

    receive
        {done, Results} ->
            shutdown_workers(Workers),
            Sum = lists:sum(Results),
            io:format("Sum of totients: ~p~n", [Sum])
    end,

    printElapsed(S, US).

% spawn_and_register_worker(ServerPid, N) ->
%     WorkerPid = spawn(fun() -> worker(ServerPid) end),
%     Name = workerName(N),
%     register(Name, WorkerPid),
%     WorkerPid.

spawn_and_register_worker(ServerPid, N) ->
    WorkerPid = spawn(fun() -> worker(ServerPid) end),
    Name = workerName(N),
    case register(Name, WorkerPid) of
        true ->
            io:format("Registered worker ~p with PID ~p~n", [Name, WorkerPid]),
            WorkerPid;
        false ->
            io:format("Failed to register worker ~p (name might be taken)~n", [Name]),
            WorkerPid
    end.

initialize_server_state(Lower, Upper, NumWorkers) ->
    #{
        current => Lower,
        upper => Upper,
        results => [],
        active_workers => NumWorkers,
        parent => self()
    }.

shutdown_workers(Workers) ->
    [WorkerPid ! {stop, self()} || WorkerPid <- Workers],
    wait_workers(length(Workers)).

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
        % worker is ready & we have work
        {ready, WorkerPid} when Current =< Upper ->
            WorkerPid ! {work, Current},
            server(State#{current := Current + 1});
        % worker is ready & we have no work
        {ready, _WorkerPid} ->
            server(State);
        % worker has completed work & we have no more work & is the last worker
        {result, Count, _WorkerPid} when Current > Upper, ActiveWorkers =:= 1 ->
            ParentPid ! {done, [Count | Results]};
        % worker has completed work & we have no more work & is not the last worker
        {result, Count, _WorkerPid} when Current > Upper ->
            server(State#{
                results := [Count | Results],
                active_workers := ActiveWorkers - 1
            });
        % worker has completed work & we have more work
        {result, Count, WorkerPid} ->
            WorkerPid ! {work, Current},
            server(State#{
                current := Current + 1,
                results := [Count | Results]
            })
    end.

worker(ServerPid) ->
    ServerPid ! {ready, self()},
    worker_loop(ServerPid).

worker_loop(ServerPid) ->
    receive
        % server has work for us
        {work, Number} ->
            Count = sequential:euler(Number),
            ServerPid ! {result, Count, self()},
            worker_loop(ServerPid);
        % parent has told us to stop
        {stop, ParentPid} ->
            ParentPid ! worker_stopped,
            ok
    end.
