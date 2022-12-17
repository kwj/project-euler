#!/bin/sh

if [ $# -eq 0 ]; then
    echo "usage: solve.sh <problem_number> [-f data_file]"
    exit 1
fi

num=$(printf "p%04d" $1)
if [ $# -eq 3 ]; then
    dune exec --display=quiet -- ${num} $2 $3
else
    dune exec --display=quiet ${num}
fi
