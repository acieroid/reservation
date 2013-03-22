#!/bin/sh
MODULE=bench
if [ -z "$CLIENTS" ]; then
  CLIENTS=4
fi
if [ -z "$ACTORS" ]; then
  ACTORS=4
fi
if [ -z "$GRIDSIZE" ]; then
  GRIDSIZE=300
fi

echo $CLIENTS
erl -noshell -s $MODULE -s init stop -gridsize $GRIDSIZE -actors $ACTORS -clients $CLIENTS
