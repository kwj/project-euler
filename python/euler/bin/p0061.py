
# project euler: problem 61

from itertools import count, dropwhile, permutations
from functools import reduce
from operator import concat
from time import perf_counter

def make_polygonal_tbl():
    def make_polygonal_lst(i):
        fn = {3: lambda n: n * (n + 1) // 2,        # triangular number
              4: lambda n: n * n,                   # square number
              5: lambda n: n * (3 * n - 1) // 2,    # pentagonal number
              6: lambda n: n * (2 * n - 1),         # hexagonal number
              7: lambda n: n * (5 * n - 3) // 2,    # heptagonal number
              8: lambda n: n * (3 * n - 2)}         # octagonal number

        lst = []
        for n in count(1):
            if (p_num := fn[i](n)) >= 10_000:
                break
            lst.append(p_num)

        return dropwhile(lambda x: x < 1_000, lst)

    tbl = dict()
    for i in range(3, 9):
        tbl[i] = list(filter(lambda tpl: tpl[1] >= 10, map(lambda n: (n // 100, n % 100), make_polygonal_lst(i))))

    return tbl

def find_cycles(p_tbl, route):
    def get_next_node(path, idx):
        return list(map(lambda next_node: [next_node] + path, filter(lambda tpl: tpl[0] == path[0][1], p_tbl[idx])))

    # Search from octagonal numbers
    paths = map(lambda tpl: [tpl], p_tbl[8])
    while len(route) > 0:
        if paths == []:
            # No next reachable node on the route
            return []
        paths = reduce(concat, map(lambda lst: get_next_node(lst, route[0]), paths))
        route = route[1:]

    # Remove non-cycles
    return list(map(lambda lst: lst[1:], filter(lambda lst: lst[0] == lst[-1], paths)))

def compute():
    # Assume that octagonal numbers are the start/goal positions on cycle
    # since number of them is the smallest
    route_patterns = map(lambda tpl: list(tpl) + [8], permutations(range(3, 8)))
    p_tbl = make_polygonal_tbl()

    # From the problem statement:
    #   - Each elements in cycle is belong to different polygonal type
    #   - There is only one cycle exist
    cycles = list(filter(lambda lst: len(lst) == len(set(lst)),
                         reduce(concat, map(lambda route: find_cycles(p_tbl, route), route_patterns))))
    if len(cycles) == 1:
        return str(sum(map(lambda tpl: 100 * tpl[0] + tpl[1], cycles[0])))

    assert False, "not reached"

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
