-module(test_sequential).
-export([run_DS1/0, run_DS2/0]).

-define(NUM_RUNS, 5).

run_DS1() ->
    run_experiment("DS1", 1, 15000).

run_DS2() ->
    run_experiment("DS2", 1, 30000).

run_experiment(Dataset, Lower, Upper) ->
    io:format(
        "Running sequential test for Dataset ~p (range: ~p to ~p)~n",
        [Dataset, Lower, Upper]
    ),

    lists:foreach(
        fun(_) ->
            {runtime, T} = sequential:sumTotient(Lower, Upper),
            io:format("~s,sequential,~.6f~n", [Dataset, T])
        end,
        lists:seq(1, ?NUM_RUNS)
    ).
