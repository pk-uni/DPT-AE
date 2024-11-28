-module(chaos).
-export([workerName/1, workerChaos/2]).

workerName(N) ->
    list_to_atom("worker" ++ integer_to_list(N)).

workerChaos(NVictims, NWorkers) ->
    lists:map(
        fun(_) ->
            %% Sleep for .5s
            timer:sleep(500),
            %% Choose a random victim
            WorkerNum = rand:uniform(NWorkers),
            io:format(
                "workerChaos killing ~p~n",
                [workerName(WorkerNum)]
            ),
            WorkerPid = whereis(workerName(WorkerNum)),
            %% Check if victim is alive
            if
                WorkerPid == undefined ->
                    io:format(
                        "workerChaos already dead: ~p~n",
                        [workerName(WorkerNum)]
                    );
                %% Kill Kill Kill
                true ->
                    exit(whereis(workerName(WorkerNum)), chaos)
            end
        end,
        lists:seq(1, NVictims)
    ).
