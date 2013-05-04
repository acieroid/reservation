#!/bin/sh
MODULE=bench
BENCHMARK=fill_with_clients
ITERATIONS=50

GRIDSIZES="50 100 250 500 750 1000 10000"
CLIENTS="100"
GRIDCOMPLETIONS="50"
SPECIFIC="60"
ACTORS=$(seq 0 1 7)

mkdir -p benchmark_results_grid

for i in $(seq $ITERATIONS); do
    echo "Iteration $i"
    for actors in $ACTORS; do
        for clients in $CLIENTS; do
            for completion in $GRIDCOMPLETIONS; do
                for specific in $SPECIFIC; do
                    for gridsize in $GRIDSIZES; do
                        echo "Starting $benchmark with actors:$actors, clients:$clients, completion:$completion, specific:$specific, gridsize:$gridsize, iteration:$i"
                        time=$(erl -noshell -s $MODULE -s init stop \
                            -benchmark $BENCHMARK \
                            -actors $actors \
                            -gridsize $gridsize \
                            -grid_completion $completion \
                            -percent_specific $specific \
                            -clients $clients)
                        echo "$BENCHMARK $actors $time" | tee -a \
                            benchmark_results/grid${gridsize}_clients${clients}_completion${completion}_specific${specific}.txt
                        echo
                    done
                done
            done
        done
    done
done

