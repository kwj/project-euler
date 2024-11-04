# project euler: problem 79

#   I saw the Wikipedia page about topological sorting.
#     https://en.wikipedia.org/wiki/Topological_sorting
#
#   Note: This implementation finds only one topological sort not all.

from collections import defaultdict
from functools import reduce
from typing import IO


def dfs(graph: dict[str, list[str]], perm: list[str], v: str) -> list[str]:
    def visit(temp: list[str], visited: list[str], node: str) -> list[str]:
        if node in temp:
            assert False
        if node in visited:
            return visited
        if node in graph:
            acc = visited
            for v in graph[node]:
                acc = visit([node] + temp, acc, v)
            return [node] + acc
        else:
            return [node]

    return visit([], perm, v)


def compute(fh: IO) -> str:
    def parse_data(fh: IO) -> dict[str, list[str]]:
        result = defaultdict(list)
        for k, v in reduce(
            lambda x, y: x + y,
            [
                [(x[0], x[1]), (x[1], x[2]), (x[0], x[2])]
                for x in fh.read().splitlines()
            ],
        ):
            result[k].append(v)
        for k, v in result.items():
            result[k] = sorted(set(v))
        return result

    graph = parse_data(fh)
    acc: list[str] = []
    for v in graph.keys():
        acc = dfs(graph, acc, v)

    return reduce(lambda x, y: x + y, acc)


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('p079_keylog.txt')
    result = compute(fh)
    fh.close()
    return result
