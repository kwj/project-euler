#!/bin/sh

if [ $# -eq 0 ]; then
    echo "usage:"
    echo "  ${0} <problem_number> [problem_number ...]"
    exit 1
fi

test_mods=""
for num in "$@"
do
    n_str=$(printf "%04d" ${num})
    test_mods="${test_mods}./tests/bin/test_p${n_str}.py "
done

echo ${test_mods} | xargs python3 -m unittest -v
