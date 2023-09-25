from collections.abc import Iterable, Iterator
from itertools import chain, combinations
from typing import Any


# Returns the largest exponent, e, for which base^e does not exceed num.
def get_max_exp(num: int, /, base: int) -> int:
    e = 0
    while num >= base:
        num //= base
        e += 1

    return e


def powerset(
    iter: Iterable[Any], /, min_len: int = 0, max_len: int = 0
) -> Iterator[Any]:
    if hasattr(iter, '__iter__') is True:
        lst = list(iter)
    else:
        assert False, 'type error'

    if max_len == 0 or max_len > len(lst):
        max_len = len(lst)

    return chain.from_iterable(
        combinations(lst, r) for r in range(min_len, max_len + 1)
    )
