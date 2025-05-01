# project euler: problem 50

from collections.abc import Iterator
from itertools import dropwhile

from euler.lib.prime import is_prime, prime_generator


def cumsum_generator() -> Iterator[int]:
    acc = 0
    p_gen = prime_generator()
    while True:
        acc += next(p_gen)
        yield acc


# Returns a cumulative sum list of prime numbers.
#   [0, p1, p1+p2, p1+p2+p3, ..., p1+...+p{n-1}, p1+...+p{n-1}+p{n}]
#     where sum(p1..p{n-1}) < limit and sum(p1..p{n}) >= limit
def init_cumsum_lst(cs_gen: Iterator[int], limit: int) -> list[int]:
    lst = [0]
    while lst[-1] < limit:
        lst.append(next(cs_gen))

    return lst


def compute(limit: int) -> str:
    cs_gen = cumsum_generator()
    cs_lst = init_cumsum_lst(cs_gen, limit)

    ans = 0
    i = 0
    consec_length = 0
    while cs_lst[i + consec_length] - cs_lst[i] < limit:
        begin = cs_lst[i]
        lst = list(
            dropwhile(
                lambda p: p - begin >= limit or not is_prime(p - begin),  # noqa: B023
                cs_lst[i + consec_length :][::-1],
            )
        )
        if lst:
            consec_length += len(lst) - 1
            ans = lst[0] - begin
        cs_lst.append(next(cs_gen))
        i += 1

    return str(ans)


def solve() -> str:
    return compute(1_000_000)
