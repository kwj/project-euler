# project euler: problem 61

from collections import defaultdict, deque
from collections.abc import Iterator
from itertools import accumulate, count, dropwhile, pairwise, takewhile


def polygonal_numbers(n: int) -> Iterator[int]:
    return filter(
        lambda x: x % 100 >= 10,
        takewhile(
            lambda x: x < 10_000,
            dropwhile(lambda x: x < 1_000, accumulate(count(1, n - 2))),
        ),
    )


def make_polygonal_tbl() -> dict[int, defaultdict[int, list[int]]]:
    result: dict[int, defaultdict[int, list[int]]] = dict()
    for i in range(3, 9):
        tbl = defaultdict(list)
        for k, v in map(lambda n: (n // 100, n % 100), polygonal_numbers(i)):
            tbl[k].append(v)
        result[i] = tbl

    return result


def find_cycles(p_tbl: dict[int, defaultdict[int, list[int]]]) -> list[list[int]]:
    def next_states(state: tuple[int, list[int]]) -> list[tuple[int, list[int]]]:
        states: list[tuple[int, list[int]]] = []
        bits, path = state

        if bits == 0b111111000 and path[0] == path[-1]:
            # a cycle is found
            cycles.append(path)
        else:
            for i in [7, 6, 5, 4, 3]:
                p_bit = 1 << i
                if p_bit & bits != 0:
                    continue
                next_key = path[-1]
                for x in p_tbl[i][next_key]:
                    states.append((bits | p_bit, path + [x]))

        return states

    cycles: list[list[int]] = []

    # Search by DFS (start from octagonal numbers)
    q: deque[tuple[int, list[int]]] = deque()
    for k, vs in p_tbl[8].items():
        for v in vs:
            q.append((0b1 << 8, [k, v]))
    while len(q) > 0:
        q.extendleft(next_states(q.popleft()))

    return cycles


def compute() -> str:
    cycles = find_cycles(make_polygonal_tbl())

    # There is only one cycle exist
    if len(cycles) == 1:
        numbers = list(map(lambda tpl: tpl[0] * 100 + tpl[1], pairwise(cycles[0])))
        # Each number is different
        if len(numbers) == len(set(numbers)):
            return str(sum(numbers))

    assert False, 'unreachable!'


def solve() -> str:
    return compute()
