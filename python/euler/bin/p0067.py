# project euler: problem 67

from collections.abc import Callable, Iterator
from functools import reduce
from operator import add
from typing import IO


def select_item(fn: Callable[..., int], lst: list[int]) -> Iterator[int]:
    return map(fn, lst, lst[1:])


def compute(fn: Callable[..., int], fh: IO) -> str:
    def parse_data(fh: IO) -> list[list[int]]:
        return [list(map(int, line.split(' '))) for line in fh.read().splitlines()]

    result = reduce(
        lambda x, y: list(map(add, select_item(fn, x), y)), reversed(parse_data(fh))
    )

    return str(result[0])


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('p067_triangle.txt')
    result = compute(max, fh)
    fh.close()
    return result
