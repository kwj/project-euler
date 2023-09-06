#!/bin/sh

if [ $# -eq 0 ]; then
    echo "usage: solve.sh <problem_number>"
    exit 1
fi

num=$(printf "p%04d" $1)
cargo run --release --bin ${num}
echo
