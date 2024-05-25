#!/bin/sh

if [ $# -eq 0 ]; then
    echo "usage: solve.sh <problem_number> [data_file]"
    exit 1
fi

if [ $# -eq 2 ]; then
    dune exec --display=quiet -- solver $1 -f $2
else
    dune exec --display=quiet -- solver $1
fi
