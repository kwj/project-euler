
# project euler: problem 67

from euler.lib.resource import asset_file
from time import perf_counter

def select_leaf(fn, lst):
    result = []
    prev = lst[0]
    for i in lst:
        result.append(fn(prev, i))
        prev = i
    return result[1:]

def compute(fn, fh):
    def parse_data(fh):
        return [list(map(int, line.split(' '))) for line in fh.read().splitlines()]

    nums = list(reversed(parse_data(fh)))
    prev = [0] * (len(nums[0]) + 1)
    for lst in nums:
        selected = select_leaf(fn, prev)
        prev = [x + y for (x, y) in zip(lst, selected)]

    return str(prev[0])

def solve():
    fh = asset_file('https://projecteuler.net/project/resources/p067_triangle.txt')
    start = perf_counter()
    result = compute(max, fh)
    elapsed_time = perf_counter() - start
    fh.close()

    return (result, "{:f}".format(elapsed_time))
