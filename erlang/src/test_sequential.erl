-module(test_sequential).
-export([
    run_sequential_experiments/0
]).

-define(DATASETS, [1500, 3000]).
-define(NUM_RUNS, 5).

% Helper function to get current time in seconds
get_time() ->
    {Mega, Sec, Micro} = os:timestamp(),
    Mega * 1000000 + Sec + Micro / 1000000.

% Run a single experiment and return the time taken
run_single_experiment(Size) ->
    Start = get_time(),
    Result = totientrange:sumTotient(1, Size),
    End = get_time(),
    TimeTaken = End - Start,
    {Result, TimeTaken}.

% Run experiment multiple times and return all times
run_multiple_experiments(Size) ->
    io:format("Running experiments for size ~p~n", [Size]),
    Times = lists:map(
        fun(_) ->
            {_Result, Time} = run_single_experiment(Size),
            io:format("Time taken: ~.6f seconds~n", [Time]),
            Time
        end,
        lists:seq(1, ?NUM_RUNS)
    ),
    {Size, Times}.

% Run experiments for all datasets
run_sequential_experiments() ->
    Results = lists:map(fun run_multiple_experiments/1, ?DATASETS),
    % Write results to file
    file:write_file("results_seq.txt", io_lib:format("~p.~n", [Results])).
