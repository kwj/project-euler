#!/bin/sh

if [ $# -eq 0 ]; then
    echo "usage: solve.sh <problem_number ...>"
    exit 1
fi

cabal run pe-solver -- $*
