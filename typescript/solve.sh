#!/bin/sh

if [ $# -eq 0 ]; then
    echo "usage: solve.sh <num [num ...]>"
    echo "         num - problem number"
    echo
    echo "example: solve.sh 1"
    exit 1
fi

deno task --config ./euler/deno.json solver $@
