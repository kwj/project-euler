# project euler: problem 82

from collections.abc import Callable
from typing import IO


def compute(fn: Callable[..., int], fh: IO) -> str:
    def parse_data(fh: IO) -> list[list[int]]:
        # transpose matrix
        return [
            list(x)
            for x in zip(
                *[list(map(int, line.split(','))) for line in fh.read().splitlines()]
            )
        ]

    matrix = parse_data(fh)
    work = matrix[0]
    for crnt in matrix[1:]:
        work[0] += crnt[0]
        for i in range(1, len(crnt)):
            work[i] = crnt[i] + fn(work[i], work[i - 1])
        for i in reversed(range(0, len(crnt) - 1)):
            work[i] = fn(work[i], work[i + 1] + crnt[i])

    return str(sorted(work)[0])


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('p082_matrix.txt')
    result = compute(min, fh)
    fh.close()
    return result
