#!/bin/sh
find benchmark_results -type f -exec Rscript ./benchmark.R {} {}.png \;
