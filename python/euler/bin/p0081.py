
# project euler: problem 81

'''
  We will need the following files to run this program.
    - https://projecteuler.net/project/resources/p081_matrix.txt
'''

from euler.lib.resource import asset_file
from itertools import accumulate
import sys
from time import perf_counter

def compute(fn, fh):
    def parse_data(fh):
        return [list(map(int, line.split(','))) for line in fh.read().splitlines()]

    matrix = parse_data(fh)

    # The 'sys.maxsize' is used as sentinel
    prev = [sys.maxsize] + list(accumulate(matrix[0]))
    for work in matrix[1:]:
        work[0:0] = [sys.maxsize]
        for i in range(1, len(work)):
            work[i] += fn(work[i - 1], prev[i])
        prev = work

    return str(prev[-1])

def solve():
    fh = asset_file('https://projecteuler.net/project/resources/p081_matrix.txt')
    start = perf_counter()
    result = compute(min, fh)
    elapsed_time = perf_counter() - start
    fh.close()

    return (result, "{:f}".format(elapsed_time))
