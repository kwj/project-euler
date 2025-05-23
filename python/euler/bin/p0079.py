# project euler: problem 79

#   I saw the Wikipedia page about topological sorting.
#     https://en.wikipedia.org/wiki/Topological_sorting
#
#   Note: This implementation finds only one topological sort not all.

from collections import defaultdict
from collections.abc import Mapping
from functools import reduce
from typing import TextIO


def dfs(graph: Mapping[str, list[str]], perm: list[str], v: str) -> list[str]:
    def visit(temp: list[str], visited: list[str], node: str) -> list[str]:
        if node in temp:
            raise AssertionError('cycle detected')
        if node in visited:
            return visited
        if node in graph:
            acc = visited
            for next_v in graph[node]:
                acc = visit([node] + temp, acc, next_v)
            return [node] + acc
        else:
            return [node]

    return visit([], perm, v)


def compute(fh: TextIO) -> str:
    def parse_data(fh: TextIO) -> dict[str, list[str]]:
        result: dict[str, list[str]] = defaultdict(list)
        for k, v in reduce(
            lambda x, y: x + y,
            [[(x[0], x[1]), (x[1], x[2]), (x[0], x[2])] for x in fh.read().splitlines()],
        ):
            result[k].append(v)

        for k, vs in result.items():
            result[k] = sorted(set(vs))

        return result

    graph = parse_data(fh)
    acc: list[str] = []
    for k in graph:
        acc = dfs(graph, acc, k)

    return reduce(lambda x, y: x + y, acc)


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('p079_keylog.txt')
    result = compute(fh)
    fh.close()
    return result
