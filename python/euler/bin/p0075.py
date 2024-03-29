# project euler: problem 75

# Pythagorean triple
#
#   a = k * (m^2 - n^2), b = k * 2mn, c = k * (m^2 + n^2)
#     where m > n > 0, gcd(m, n) = 1
#
#   perimeter L = k * (2m^2 + 2mn)
#               = k * 2m(m + n)
#
#   2m(m + n) = L/k
#     -->
#   2m^2 < 2m(m + n) = L/k
#     <-->
#   m^2 < L/2k
#
#   'm' is maximized when k=1
#     max(m) < sqrt(L/2)

from collections import Counter
from math import gcd, isqrt


def compute(L: int) -> str:
    limit = isqrt(L // 2)
    counter: Counter[int] = Counter()

    for m in range(2, limit + 1):
        for n in range(1 + (m % 2), m, 2):
            if gcd(m, n) == 1:
                if (perimeter := 2 * m * (m + n)) > L:
                    break
                for p in range(perimeter, L + 1, perimeter):
                    counter[p] += 1

    return str(sum(1 for n in counter.values() if n == 1))


def solve() -> str:
    return compute(1_500_000)
