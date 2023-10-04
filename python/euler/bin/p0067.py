# project euler: problem 67

from collections.abc import Callable
from typing import IO


def select_leaf(fn: Callable[..., int], lst: list[int]) -> list[int]:
    result = []
    prev = lst[0]
    for i in lst:
        result.append(fn(prev, i))
        prev = i
    return result[1:]


def compute(fn: Callable[..., int], fh: IO) -> str:
    def parse_data(fh: IO) -> list[list[int]]:
        return [list(map(int, line.split(' '))) for line in fh.read().splitlines()]

    nums = list(reversed(parse_data(fh)))
    prev = nums[0]
    for lst in nums[1:]:
        selected = select_leaf(fn, prev)
        prev = [x + y for (x, y) in zip(lst, selected)]

    return str(prev[0])


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('https://projecteuler.net/project/resources/p067_triangle.txt')
    result = compute(max, fh)
    fh.close()
    return result
