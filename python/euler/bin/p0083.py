
# project euler: problem 83

'''
  I used Dijkstra's algorithm to solve.
    https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

  We will need the following files to run this program.
    - https://projecteuler.net/project/resources/p083_matrix.txt
'''

from euler.lib.resource import asset_file
from euler.lib.util import HeapQueue
import sys
from time import perf_counter

def make_neighbor_tbl(rows, columns):
    tbl = [[0] * columns for _ in range(rows)]
    for r in range(rows):
        for c in range(columns):
            tbl[r][c] = list(filter(lambda tpl: tpl[0] >= 0 and tpl[0] < rows and tpl[1] >= 0 and tpl[1] < columns,
                            [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]))

    return tbl

def make_distance_tbl(rows, columns):
    return [[sys.maxsize] * columns for _ in range(rows)]

def compute(fn, fh):
    def parse_data(fh):
        return [list(map(int, line.split(','))) for line in fh.read().splitlines()]

    matrix = parse_data(fh)
    nbr_tbl = make_neighbor_tbl(rows=len(matrix), columns=len(matrix[0]))
    dist_tbl = make_distance_tbl(rows=len(matrix), columns=len(matrix[0]))
    dist_tbl[0][0] = matrix[0][0]
    pq = HeapQueue()
    pq.insert((dist_tbl[0][0], (0, 0)))

    while pq.is_empty() == False:
        d, (i, j) = pq.extract()
        for x, y in nbr_tbl[i][j]:
            if (new_d := d + matrix[x][y]) < dist_tbl[x][y]:
                dist_tbl[x][y] = new_d
                pq.insert((new_d, (x, y)))

    return str(dist_tbl[-1][-1])

def solve():
    fh = asset_file('https://projecteuler.net/project/resources/p083_matrix.txt')
    start = perf_counter()
    result = compute(min, fh)
    elapsed_time = perf_counter() - start
    fh.close()

    return (result, "{:f}".format(elapsed_time))