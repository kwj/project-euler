# project euler: problem 99

#   log10 base^exp = exp * (log10 base)
#
#   We will need the following file to run this program.
#     - https://projecteuler.net/project/resources/p099_base_exp.txt

from collections.abc import Iterator
from math import log10
from typing import IO


def compute(fh: IO) -> str:
    def parse_data(fh: IO) -> Iterator[tuple[int, int]]:
        return map(
            lambda lst: (int(lst[0]), int(lst[1])),
            [line.split(',') for line in fh.read().splitlines()],
        )

    calc_result = map(lambda tpl: tpl[1] * log10(tpl[0]), parse_data(fh))

    return str(sorted(enumerate(calc_result), key=lambda x: x[1])[-1][0] + 1)


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('p099_base_exp.txt')
    result = compute(fh)
    fh.close()
    return result
