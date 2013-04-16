#!/bin/sh
MODULE=bench
BENCHMARK=fill_with_clients
ITERATIONS=50

GRIDSIZES=$(seq 200 50 200)
CLIENTS="1 5 10 100 1000"
GRIDCOMPLETIONS=$(seq 25 25 100)
SPECIFIC=$(seq 25 25 100)
ACTORS=$(seq 0 1 7)

mkdir -p benchmark_results

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
                            benchmark_results/grid${gridsize}_clients${clients}_completion${completion}.txt
                        echo
                    done
                done
            done
        done
    done
done

