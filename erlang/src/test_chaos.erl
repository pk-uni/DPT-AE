-module(test_chaos).
-export([run_experiment/0]).

-define(DATASETS, [1500, 3000, 6000]).
-define(CHAOS_WORKERS, 4).
-define(CHAOS_VICTIMS, 3).

run_experiment() ->
    io:format("Running Chaos Experiments~n"),
    io:format("================================~n"),

    Size = lists:nth(2, ?DATASETS),
    io:format("Dataset Size: ~p with ~p workers~n", [Size, ?CHAOS_WORKERS]),
    io:format("Will kill ~p workers during execution~n", [?CHAOS_VICTIMS]),

    parallel:start(1, Size, ?CHAOS_WORKERS),

    timer:sleep(1000),
    chaos:workerChaos(?CHAOS_VICTIMS, ?CHAOS_WORKERS),

    timer:sleep(Size * 5).
