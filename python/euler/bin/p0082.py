
# project euler: problem 82

'''
  We will need the following files to run this program.
    - https://projecteuler.net/project/resources/p082_matrix.txt
'''

from euler.lib.resource import asset_file
from time import perf_counter

def compute(fn, fh):
    def parse_data(fh):
        # transpose matrix
        return [list(x) for x in zip(*[list(map(int, line.split(','))) for line in fh.read().splitlines()])]

    matrix = parse_data(fh)
    work = matrix[0]
    for crnt in matrix[1:]:
        work[0] += crnt[0]
        for i in range(1, len(crnt)):
            work[i] = crnt[i] + fn(work[i], work[i - 1])
        for i in reversed(range(0, len(crnt) - 1)):
            work[i] = fn(work[i], work[i + 1] + crnt[i])

    return str(sorted(work)[0])

def solve():
    fh = asset_file('https://projecteuler.net/project/resources/p082_matrix.txt')
    start = perf_counter()
    result = compute(min, fh)
    elapsed_time = perf_counter() - start
    fh.close()

    return (result, "{:f}".format(elapsed_time))
