# project euler: problem 50

from collections.abc import Generator
from itertools import dropwhile

from euler.lib.prime import is_prime, prime_generator


def cumsum_generator() -> Generator[int, None, None]:
    acc = 0
    p_gen = prime_generator()
    while True:
        acc += next(p_gen)
        yield acc


# Returns a cumulative sum list of prime numbers.
#   [0, p1, p1+p2, p1+p2+p3, ..., p1+...+p{n-1}, p1+...+p{n-1}+p{n}]
#     where sum(p1..p{n-1}) < limit and sum(p1..p{n}) >= limit
def init_cumsum_lst(cs_gen: Generator[int, None, None], limit: int) -> list[int]:
    lst = [0]
    while lst[-1] < limit:
        lst.append(next(cs_gen))

    return lst


def compute(limit: int) -> str:
    assert limit > 2, 'upper limit must be larger than 2'
    cs_gen = cumsum_generator()
    cs_lst = init_cumsum_lst(cs_gen, limit)

    k = len(cs_lst) - 2
    left = 0
    while True:
        diff = cs_lst[left + k] - cs_lst[left]
        if diff >= limit:
            left = 0
            k -= 1
        elif is_prime(diff):
            return str(diff)
        else:
            left += 1
            if left + k >= len(cs_lst):
                cs_lst.append(next(cs_gen))

    raise RuntimeError('unreachable!')


def solve() -> str:
    return compute(1_000_000)
