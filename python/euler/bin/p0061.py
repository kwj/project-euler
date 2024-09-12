# project euler: problem 61

# Please see the following link for reasons to ignore type checking.
# https://github.com/python/mypy/issues/4673

from collections.abc import Iterator
from functools import reduce
from itertools import accumulate, count, dropwhile, permutations, takewhile
from operator import concat


def make_polygonal_tbl() -> dict[int, list[tuple[int, int]]]:
    def polygonal_numbers(n: int) -> Iterator[int]:
        return filter(
            lambda x: x % 100 >= 10,
            takewhile(
                lambda x: x < 10_000,
                dropwhile(lambda x: x < 1_000, accumulate(count(1, n - 2))),
            ),
        )

    tbl = dict()
    for i in range(3, 9):
        tbl[i] = list(map(lambda n: (n // 100, n % 100), polygonal_numbers(i)))

    return tbl


def find_cycles(
    p_tbl: dict[int, list[tuple[int, int]]], route: list[int]
) -> list[list[tuple[int, int]]]:
    def get_next_node(
        path: list[tuple[int, int]], idx: int
    ) -> list[list[tuple[int, int]]]:
        return list(
            map(
                lambda next_node: [next_node] + path,
                filter(lambda tpl: tpl[0] == path[0][1], p_tbl[idx]),
            )
        )

    # Search from octagonal numbers
    paths = list(map(lambda tpl: [tpl], p_tbl[8]))
    while len(route) > 0:
        if paths == []:
            # No next reachable node on the route
            return []
        paths = reduce(concat, map(lambda lst: get_next_node(lst, route[0]), paths))  # type: ignore
        route = route[1:]

    # Remove non-cycles
    return list(map(lambda lst: lst[1:], filter(lambda lst: lst[0] == lst[-1], paths)))


def compute() -> str:
    # Assume that octagonal numbers are the start/goal positions on cycle
    # since number of them is the smallest
    route_patterns = map(lambda tpl: list(tpl) + [8], permutations(range(3, 8)))
    p_tbl = make_polygonal_tbl()

    # From the problem statement:
    #   - Each elements in cycle is belong to different polygonal type
    #   - There is only one cycle exist
    cycles: list[list[tuple[int, int]]] = reduce(
        concat, map(lambda route: find_cycles(p_tbl, route), route_patterns)  # type: ignore
    )
    cycles = list(filter(lambda lst: len(lst) == len(set(lst)), cycles))

    if len(cycles) == 1:
        return str(sum(map(lambda tpl: 100 * tpl[0] + tpl[1], cycles[0])))
    else:
        assert False, 'unreachable!'


def solve() -> str:
    return compute()
