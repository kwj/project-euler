# project euler: problem 26

from math import lcm

from euler.lib.util import divisors, factorize


def pp(n: int) -> int:
    while n % 2 == 0:
        n //= 2
    while n % 5 == 0:
        n //= 5

    return n


def carmichael(n: int) -> int:
    # This function is not strictly the correct Carmichael function
    # because the function assumes that the argument is not a multiple of 2.
    return lcm(*map(lambda x: (x[0] - 1) * (x[0] ** (x[1] - 1)), factorize(n)))


def find_repetend_length(d: int) -> int:
    if (d := pp(d)) == 1:
        return 0

    for k in divisors(carmichael(d)):
        if pow(10, k, d) == 1:
            return k

    raise RuntimeError('unreachable!')


def compute(limit: int) -> str:
    max_length = 0
    answer = 0
    for i in reversed(range(limit // 2, limit)):
        if i <= max_length:
            break
        repetend_length = find_repetend_length(i)
        if repetend_length > max_length:
            answer = pp(i)
            max_length = repetend_length

    return str(answer)


def solve() -> str:
    return compute(1_000)
