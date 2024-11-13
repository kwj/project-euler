# project euler: problem 96

#   [Grid]
#       C0 C1 C2 C3 C4 C5 C6 C7 C8
#      +--------+--------+--------+    R: Row
#    R0|        |        |        |    C: Column
#    R1|   B0   |   B1   |   B2   |    B: Box
#    R2|        |        |        |
#      +--------+--------+--------+    Cell = RxCy
#    R3|        |        |        |
#    R4|   B3   |   B4   |   B5   |
#    R5|        |        |        |
#      +--------+--------+--------+
#    R6|        |        |        |
#    R7|   B6   |   B7   |   B8   |
#    R8|        |        |        |
#      +--------+--------+--------+

import re
from functools import reduce
from typing import IO

from euler.lib.util import flatten

# row/column/position(cell)
ROW = ['R' + str(i) for i in range(9)]  # R0, R1, ..., R8
COL = ['C' + str(i) for i in range(9)]  # C0, C1, ..., C8
POS = [r + c for r in ROW for c in COL]  # R0C0, R0C1, ..., R8C8

# list of cells belonging to row/column/box
ROW_LST = [[r + c for c in COL] for r in ROW]
COL_LST = [[r + c for r in ROW] for c in COL]
BOX_LST = [
    [r + c for r in rows for c in cols]
    for rows in (ROW[:3], ROW[3:6], ROW[6:])
    for cols in (COL[:3], COL[3:6], COL[6:])
]

# dictionary of cells and their corresponding row, column and box
groups = dict(
    (p, [group for group in ROW_LST + COL_LST + BOX_LST if p in group]) for p in POS
)
# dictionary of cells and their corresponding cells
links = dict((p, set(flatten(groups[p])) - set((p,))) for p in POS)


class Grid:
    def __init__(self, data: str):
        self._data = data

    # We make a grid as dictionary which values are immutable data, string.
    # The result is an easy backtracked search with a shallow copy of the dictionary.
    def setup_grid(self) -> bool:
        self._grid = dict((p, '123456789') for p in POS)
        for pos, ch in zip(POS, self._data):
            if ch != '0':
                if self._decide_num(self._grid, pos, ch) is None:
                    return False

        return True

    # Tentatively determine a number to leave in the cell and remove the other numbers
    def _decide_num(
        self, grid: dict[str, str], pos: str, num: str
    ) -> dict[str, str] | None:
        # Remove all numbers from the cell except the one we have decided on
        other_nums = grid[pos].replace(num, '')
        if all(self._remove_num(grid, pos, n) for n in other_nums):
            return grid
        else:
            return None

    # Remove the number from the candidate in the cell and update involved cells
    #
    # Note: After processing this method, the calling variable 'grid' is updated
    #       since this dictionary parameter is not copied from the original
    #       but is a reference to the original.
    def _remove_num(self, grid: dict[str, str], pos: str, num: str) -> bool:
        # If the target number to remove is not already in the cell, do nothing
        if num not in grid[pos]:
            return True
        # If there is only the target number in the cell to be removed, it is a contradiction
        if grid[pos] == num:
            return False

        grid[pos] = grid[pos].replace(num, '')
        # If the deletion results only one number is in the cell,
        # remove the number from the linked cells
        if len(grid[pos]) == 1:
            if (
                all(self._remove_num(grid, p, grid[pos]) for p in links[pos])
                is not True
            ):
                return False

        # check row/column/box
        for group in groups[pos]:
            # If the target number doesn't remain in a belong row/column/box,
            # it is a contradiction.
            cells = [sq for sq in group if num in grid[sq]]
            if not cells:
                return False
            # If there is an only one cell which contains the removed number
            # in a belong row/column/box, the number is decided tentatively
            # to remain in the cell
            if len(cells) == 1:
                if self._decide_num(grid, cells[0], num) is None:
                    return False

        # everything is OK
        return True

    def _solve(self, grid: dict[str, str] | None) -> dict[str, str] | None:
        if grid is None:
            return None

        cells = [(len(grid[p]), p) for p in POS]
        if all(c[0] == 1 for c in cells):
            return grid

        _, pos = min((c for c in cells if c[0] > 1), key=lambda x: x[0])
        for num in grid[pos]:
            if (
                result := self._solve(self._decide_num(grid.copy(), pos, num))
            ) is not None:
                return result

        return None

    def solve(self) -> dict[str, str] | None:
        return self._solve(self._grid)


def parse_data(fh: IO) -> list[str]:
    def trim(lst):
        s = reduce(lambda x, y: x + y, lst)
        tmp = re.sub(r'[^0-9.]', '', s)
        return re.sub(r'\.', '0', tmp)

    result = []
    acc = []
    for line in fh.read().splitlines():
        if re.match(r'[0-9.]', line) is not None:
            acc.append(line)
        elif re.match(r'-', line) is not None or not acc:
            continue
        else:
            result.append(trim(acc))
            acc = []
    else:
        if len(acc) == 9:
            result.append(trim(flatten(acc)))

    assert all([len(s) == 81 for s in result]), 'invalid data file'
    return result


def compute(fh: IO) -> str:
    puzzles = parse_data(fh)
    acc = 0
    for idx, problem in enumerate(puzzles):
        grid = Grid(problem)
        if not grid.setup_grid():
            assert False, 'invalid data: Grid {}'.format(idx + 1)

        d = grid.solve()
        if d is not None:
            acc += int(d['R0C0'] + d['R0C1'] + d['R0C2'])
        else:
            print('Warning: No answer found: Grod {}'.format(idx + 1))

    return str(acc)


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('p096_sudoku.txt')
    result = compute(fh)
    fh.close()
    return result
