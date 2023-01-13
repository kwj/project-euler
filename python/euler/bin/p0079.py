
# project euler: problem 79

'''
  If you look at the log file, you can assume the answer with pen and paper :).

  We'll need the following files to run this program.
    - https://projecteuler.net/project/resources/p079_keylog.txt

  I saw the Wikipedia page about topological sorting.
    https://en.wikipedia.org/wiki/Topological_sorting

  Note: This implementation finds only one topological sort not all.
'''

from euler.lib.resource import asset_file
from collections import defaultdict
from functools import reduce
from time import perf_counter

def dfs(graph, perm, v):
    def visit(temp, visited, node):
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

def compute(fh):
    def parse_data(fh):
        result = defaultdict(list)
        for k, v in reduce(lambda x, y: x + y, [[(x[0], x[1]), (x[1], x[2]), (x[0], x[2])] for x in fh.read().splitlines()]):
            result[k].append(v)
        for k, v in result.items():
            result[k] = sorted(set(v))
        return result

    graph = parse_data(fh)
    acc = []
    for v in graph.keys():
        acc = dfs(graph, acc, v)

    return reduce(lambda x, y: x + y, acc)

def solve():
    fh = asset_file('https://projecteuler.net/project/resources/p079_keylog.txt')
    start = perf_counter()
    result = compute(fh)
    elapsed_time = perf_counter() - start
    fh.close()

    return (result, "{:f}".format(elapsed_time))

