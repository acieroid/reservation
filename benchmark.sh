#!/bin/sh
MODULE=bench
BENCHMARK=fill_with_clients
ITERATIONS=100

GRIDSIZES=$(seq 50 50 500)
CLIENTS=$(seq 1 2 10)
GRIDCOMPLETIONS=$(seq 10 10 100)
ACTORS=$(seq 1 1 8)

mkdir -p benchmark_results

for i in $(seq $ITERATIONS); do
    echo "Iteration $i"
    for actors in $ACTORS; do
        for clients in $CLIENTS; do
            for completion in $GRIDCOMPLETIONS; do
                for gridsize in $GRIDSIZES; do
                    echo "Starting $benchmark with actors:$actors, clients:$clients, completion:$completion, gridsize:$gridsize"
                    time=$(erl -noshell -s $MODULE -s init stop \
                        -benchmark $BENCHMARK \
                        -actors $actors \
                        -gridsize $gridsize \
                        -grid_completion $completion \
                        -clients $clients)
                    echo "$BENCHMARK $actors $time" | tee -a \
                        benchmark_results/grid${gridsize}_clients${client}_completion${completion}.txt
                done
            done
        done
    done
done


