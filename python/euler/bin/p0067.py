# project euler: problem 67

from collections.abc import Callable
from functools import reduce
from itertools import pairwise
from operator import add
from typing import IO


def parse_data(fh: IO) -> list[list[int]]:
    return [list(map(int, line.split(' '))) for line in fh.read().splitlines()]


def compute(fn: Callable[..., int], fh: IO) -> str:
    return str(
        reduce(
            lambda x, y: list(map(add, map(fn, pairwise(x)), y)),
            reversed(parse_data(fh)),
        )[0]
    )


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('p067_triangle.txt')
    result = compute(max, fh)
    fh.close()
    return result
