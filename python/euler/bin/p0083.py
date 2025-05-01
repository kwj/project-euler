# project euler: problem 83

#   I used Dijkstra's algorithm to solve.
#     https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm

import sys
from typing import IO

from euler.lib.util import HeapQueue


def make_neighbor_tbl(rows: int, columns: int) -> list[list[list[tuple[int, int]]]]:
    tbl: list[list[list[tuple[int, int]]]] = [[[(0, 0)]] * columns for _ in range(rows)]
    for r in range(rows):
        for c in range(columns):
            tbl[r][c] = list(
                filter(
                    lambda tpl: tpl[0] >= 0 and tpl[0] < rows and tpl[1] >= 0 and tpl[1] < columns,
                    [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)],
                )
            )

    return tbl


def make_distance_tbl(rows: int, columns: int) -> list[list[int]]:
    return [[sys.maxsize] * columns for _ in range(rows)]


def compute(fh: IO) -> str:
    def parse_data(fh: IO) -> list[list[int]]:
        return [list(map(int, line.split(','))) for line in fh.read().splitlines()]

    matrix = parse_data(fh)
    nbr_tbl = make_neighbor_tbl(rows=len(matrix), columns=len(matrix[0]))
    dist_tbl = make_distance_tbl(rows=len(matrix), columns=len(matrix[0]))
    dist_tbl[0][0] = matrix[0][0]
    pq = HeapQueue()
    pq.insert((dist_tbl[0][0], (0, 0)))

    while not pq.is_empty():
        d, (i, j) = pq.extract()
        for x, y in nbr_tbl[i][j]:
            if (new_d := d + matrix[x][y]) < dist_tbl[x][y]:
                dist_tbl[x][y] = new_d
                pq.insert((new_d, (x, y)))

    return str(dist_tbl[-1][-1])


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('p083_matrix.txt')
    result = compute(fh)
    fh.close()
    return result
