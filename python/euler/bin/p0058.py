# project euler: problem 58

#   37 36 35 34 33 32 31
#   38 17 16 15 14 13 30
#   39 18  5  4  3 12 29
#   40 19  6  1  2 11 28
#   41 20  7  8  9 10 27
#   42 21 22 23 24 25 26
#   43 44 45 46 47 48 49
#             |  |  |  |
#          (n=0, 1, 2, 3, ...)
#
#   length of edge:
#     2n+1
#
#   sum of numbers lying along both diagonals:
#     4n + 1
#
#   each numbers in the four corners: (P:Prime, C:Composite)
#
#   n=0  1,
#   n=1        (1+2),     (1+2+2),     (1+2+2+2),     a1=(1+2+2+2+2),
#   n=2        (a1+4),    (a1+4+4),    (a1+4+4+4),    a2=(a1+4+4+4+4),
#   n=3        (a2+6),    (a2+6+6),    (a2+6+6+6),    a3=(a2+6+6+6+6),
#              ...
#   0-----------------------------------------------------------------
#       NonP,  P or C     P or C       P or C         C (square number)
#               d{n}       c{n}          b{n}             a{n}
#
#     a{n} = (2n + 1) ^ 2 = 4n^2 + 4n + 1
#     b{n} = a{n} - 2n = 4n^2 + 2n + 1
#     c{n} = a{n} - 4n = 4n^2 + 1
#     d{n} = a{n} - 6n = 4n^2 - 2n + 1
#
#   assume that m = 2n + 1
#
#     b{n} = m(m-1) + 1
#     c{n} = m(m-2) + 2
#     d{n} = m(m-3) + 3
#
#     length of edge = 2n + 1 = m
#     sum of numbers lying along both diagonals = 4n + 1 = 2m - 1

from itertools import count

from euler.lib.prime import is_prime


def compute() -> str:
    def wrapper(n: int) -> int:
        if is_prime(n) is True:
            return 1
        else:
            return 0

    n_primes = 0
    for m in count(3, 2):
        n_primes += wrapper(m * (m - 1) + 1)
        n_primes += wrapper(m * (m - 2) + 2)
        n_primes += wrapper(m * (m - 3) + 3)
        if n_primes * 10 < 2 * m - 1:
            return str(m)

    assert False, 'unreachable!'


def solve() -> str:
    return compute()
