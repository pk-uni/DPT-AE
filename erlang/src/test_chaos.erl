-module(test_chaos).
-export([run_experiment/2]).
-import(chaos, [workerChaos/2]).
-import(parallel, [start/3]).

% test harness

run_experiment(NumWorkers, NumVictims) ->
    {runtime, _Time, _Result} = parallel:start(1, 60000, NumWorkers),

    chaos:workerChaos(NumVictims, NumWorkers).
