# project euler: problem 77

from collections.abc import Iterator

from euler.lib.prime import prime_generator


def plst_generator() -> Iterator[list[int]]:
    p_gen = prime_generator()
    plst = []
    while True:
        plst.append(next(p_gen))
        yield plst


def compute(boundary: int) -> str:
    plst_gen = plst_generator()
    while True:
        plst = next(plst_gen)
        tbl = [0] * (len(plst) + 1)
        tbl[0] = 1
        for i in plst:
            # Skip when i >= len(tbl)
            # This is because the prime number 'i' is larger than the sum of primes we now assume
            for j in range(i, len(tbl)):
                tbl[j] += tbl[j - i]

        # over the boundary
        if tbl[-1] > boundary:
            break

    return str(len(plst))


def solve() -> str:
    return compute(5_000)
