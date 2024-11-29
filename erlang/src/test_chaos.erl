-module(test).
-export([run_chaos_experiments/0]).

-define(DATASETS, [1500, 3000, 6000]).
-define(CHAOS_WORKERS, 4).
-define(CHAOS_VICTIMS, 3).

% Runs the chaos testing experiments
run_chaos_experiments() ->
    io:format("Running Chaos Experiments~n"),
    io:format("================================~n"),
    % Start with a medium-sized dataset
    Size = lists:nth(2, ?DATASETS),
    io:format("Dataset Size: ~p with ~p workers~n", [Size, ?CHAOS_WORKERS]),
    io:format("Will kill ~p workers during execution~n", [?CHAOS_VICTIMS]),

    parallel:start(1, Size, ?CHAOS_WORKERS),
    % Wait a bit before introducing chaos
    timer:sleep(1000),
    chaos:workerChaos(?CHAOS_VICTIMS, ?CHAOS_WORKERS),
    % Wait for computation to complete
    timer:sleep(Size * 5).
