#!/bin/sh
MODULE=bench
BENCHMARKS=fill_with_clients
ITERATIONS=100

ACTORS_START=1
ACTORS_STEP=1
ACTORS_STOP=8

for i in $(seq $ITERATIONS); do
    echo "Iteration $i"
    for actors in $(seq $ACTORS_START $ACTORS_STEP $ACTORS_STOP); do
        for benchmark in $BENCHMARKS; do
            echo "Starting $benchmark with $actors actors"
            time=$(erl -noshell -s $MODULE -s init stop \
                -benchmark $benchmark \
                -actors $actors)
            echo "$benchmark $actors $time" | tee -a benchmark_results.txt
        done
    done
done
