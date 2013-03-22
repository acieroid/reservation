#!/bin/sh
MODULE=bench
ITERATIONS=100
erl -noshell -s $MODULE -s init stop | tee -a bench_results.txt
