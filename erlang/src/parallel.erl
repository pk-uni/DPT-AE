-module(parallel).
-import(sequential, [euler/1, printElapsed/2]).
-import(chaos, [workerName/1]).
-export([start/3]).

start(Lower, Upper, NumWorkers) ->
    StartPid = self(),
    spawn(fun() ->
        io:format("Starting program with PID ~p~n", [self()]),
        MainPid = self(),
        % setup the server and supervisor
        ServerPid = spawn(fun() -> server(initialize_server_state(Lower, Upper, MainPid)) end),
        SupervisorPid = spawn(fun() -> supervisor(ServerPid, NumWorkers) end),

        % starts the computation
        sumTotient(SupervisorPid, MainPid),

        receive
            {done, Time} ->
                io:format("runtime:~p~n", [Time]),
                StartPid ! {runtime, Time},
                ok
        end
    end),

    receive
        {runtime, T} -> {runtime, T}
    end.

sumTotient(SupervisorPid, ParentPid) ->
    {_, S, US} = os:timestamp(),

    receive
        {done, Results} ->
            % tell supervisor to shut down workers
            SupervisorPid ! shutdown,
            Sum = lists:sum(Results),
            io:format("Sum of totients: ~p~n", [Sum])
    end,

    Time = printElapsed(S, US),
    ParentPid ! {done, Time}.

supervisor(ServerPid, NumWorkers) ->
    % start workers and enter main loop
    WorkerInfo = start_workers(ServerPid, NumWorkers, #{}),
    supervisor_loop(ServerPid, NumWorkers, WorkerInfo).

supervisor_loop(ServerPid, NumWorkers, WorkerInfo) ->
    receive
        {'DOWN', Ref, process, OldPid, Reason} ->
            case maps:get(Ref, WorkerInfo, not_found) of
                % should not be reachable
                not_found ->
                    supervisor_loop(ServerPid, NumWorkers, WorkerInfo);
                % worker N has died
                N ->
                    io:format("Worker ~p has died due to ~p~n", [N, Reason]),
                    maps:remove(Ref, WorkerInfo),

                    {NewPid, NewRef} = start_worker(ServerPid, N),
                    NewWorkerInfo = WorkerInfo#{NewRef => N},

                    ServerPid ! {worker_restarted, OldPid, NewPid},

                    supervisor_loop(ServerPid, NumWorkers, NewWorkerInfo)
            end;
        shutdown ->
            % kill all workers
            maps:foreach(
                fun(_Ref, WorkerNum) ->
                    WorkerPid = whereis(workerName(WorkerNum)),
                    WorkerPid ! {stop, self()}
                end,
                WorkerInfo
            )
    end.

start_workers(ServerPid, NumWorkers, WorkerInfo) ->
    % start and track workers in a map
    lists:foldl(
        fun(N, Acc) ->
            {_WorkerPid, Ref} = start_worker(ServerPid, N),
            maps:put(Ref, N, Acc)
        end,
        WorkerInfo,
        lists:seq(1, NumWorkers)
    ).

start_worker(ServerPid, WorkerNum) ->
    % start a worker and register it
    Pid = spawn(fun() -> worker(ServerPid) end),

    % unregister name if it is already registered
    WorkerName = workerName(WorkerNum),
    case whereis(WorkerName) of
        undefined -> ok;
        _ -> unregister(WorkerName)
    end,
    register(WorkerName, Pid),

    Ref = erlang:monitor(process, Pid),
    {Pid, Ref}.

initialize_server_state(Lower, Upper, ParentPid) ->
    #{
        current => Lower,
        upper => Upper,
        results => [],
        worker_tasks => #{},
        parent => ParentPid
    }.

server(
    #{
        current := Current,
        upper := Upper,
        results := Results,
        worker_tasks := WorkerTasks,
        parent := ParentPid
    } = State
) ->
    receive
        % worker is ready & we have work
        {ready, WorkerPid} when Current =< Upper ->
            NewWorkerTasks = maps:put(WorkerPid, Current, WorkerTasks),
            WorkerPid ! {work, Current},
            server(State#{
                current := Current + 1,
                worker_tasks := NewWorkerTasks
            });
        % worker is ready & we have no work
        {ready, _WorkerPid} ->
            server(State);
        % worker has completed work & we have no more work
        {result, Count, WorkerPid} when Current > Upper ->
            NewWorkerTasks = maps:remove(WorkerPid, WorkerTasks),

            case maps:size(NewWorkerTasks) of
                %  is the last worker
                0 ->
                    ParentPid ! {done, [Count | Results]};
                %  is not the last worker
                _ ->
                    server(State#{
                        results := [Count | Results],
                        worker_tasks := NewWorkerTasks
                    })
            end;
        % worker has completed work & we have more work
        {result, Count, WorkerPid} ->
            NewWorkerTasks = maps:remove(WorkerPid, WorkerTasks),
            WorkerPid ! {work, Current},
            server(State#{
                current := Current + 1,
                results := [Count | Results],
                worker_tasks := maps:put(WorkerPid, Current, NewWorkerTasks)
            });
        % worker has been restarted
        {worker_restarted, OldPid, NewPid} ->
            % if the worker was working on a task, reassign it to the new worker
            case maps:get(OldPid, WorkerTasks, none) of
                none ->
                    server(State);
                TaskNum ->
                    NewWorkerTasks = maps:remove(OldPid, WorkerTasks),
                    NewPid ! {work, TaskNum},
                    server(State#{worker_tasks := maps:put(NewPid, TaskNum, NewWorkerTasks)})
            end
    end.

worker(ServerPid) ->
    % tell server we are ready
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
