#!/bin/sh
module="${1%.*}"
erl -noshell -s $module $2 -s init stop
