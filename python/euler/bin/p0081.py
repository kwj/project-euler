# project euler: problem 81

#   We will need the following files to run this program.
#     - https://projecteuler.net/project/resources/p081_matrix.txt

import sys
from collections.abc import Callable
from itertools import accumulate
from typing import IO


def compute(fn: Callable[..., int], fh: IO) -> str:
    def parse_data(fh: IO) -> list[list[int]]:
        return [list(map(int, line.split(','))) for line in fh.read().splitlines()]

    matrix = parse_data(fh)

    # The 'sys.maxsize' is used as sentinel
    prev = [sys.maxsize] + list(accumulate(matrix[0]))
    for work in matrix[1:]:
        work[0:0] = [sys.maxsize]
        for i in range(1, len(work)):
            work[i] += fn(work[i - 1], prev[i])
        prev = work

    return str(prev[-1])


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('https://projecteuler.net/project/resources/p081_matrix.txt')
    result = compute(min, fh)
    fh.close()
    return result
