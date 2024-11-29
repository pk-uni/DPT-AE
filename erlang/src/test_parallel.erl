-module(test).
-export([run_parallel_experiments/0]).

-define(DATASETS, [1500, 3000, 6000]).
-define(DEFAULT_WORKERS, 4).

% Runs the parallel implementation across all datasets
run_parallel_experiments() ->
    io:format("Running Parallel Experiments~n"),
    io:format("================================~n"),
    lists:foreach(
        fun(Size) ->
            io:format("Dataset Size: ~p with ~p workers~n", [Size, ?DEFAULT_WORKERS]),
            parallel:start(1, Size, ?DEFAULT_WORKERS),
            % Give enough time for the parallel computation to complete
            timer:sleep(Size * 5)
        end,
        ?DATASETS
    ).
