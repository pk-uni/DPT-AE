-module(test_chaos).
-export([run_t1/0, run_t2/0, run_t3/0]).
-import(chaos, [workerChaos/2]).
-import(parallel, [start/3]).

% test harness

run_t1() ->
    run_experiment(4, 3).

run_t2() ->
    run_experiment(8, 6).

run_t3() ->
    run_experiment(12, 10).

run_experiment(NumWorkers, NumVictims) ->
    {runtime, Time, Result} = parallel:start(1, 60000, NumWorkers),

    chaos:workerChaos(NumVictims, NumWorkers),
    io:format("~p,~p,~p,~p~n", [NumWorkers, NumVictims, Result, Time]),
    {NumWorkers, NumVictims, Result, Time}.
