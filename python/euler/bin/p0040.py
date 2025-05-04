# project euler: problem 40

#   0.123456789 | 10111213...979899 | 100101102...997998999  | 100010011002  ...
#     ---------   -----------------   ---------------------   -----------------
# len: 1 * 9       2 * 90              3 * 900                 4 * 9000      ...
#      1 * 9 * 1   2 * 9 * 10          3 * 9 * 100             4 * 9 * 1000  ...
#        --> block_num * 9 * base
#
#  block #1: 1-digit number
#  block #2: 2-digits number
#  block #3: 3-digits number
#    ...
#  block #n: n-digits number

from typing import cast


#  0.123456789 | 10111213...979899 | 100101102...997998999  | 100010011002  ...
#                     ^[d15]=2
#  example d(15)
#    pos = 15 > 1 * 9 * (10 ** 0) = 9
#    pos <- 15 - 9 = 6
#
#    pos = 6 <= 2 * 9 * (10 ** 1) = 180
#    q <- (6 - 1) / 2 = 2, r <- (6 - 1) % 2 = 1
#    num <- 10 ** (2 - 1) + q = 10 + 2 = 12
#    d[15] = num / (10 ** (2 - r - 1)) % 10
#          = 12 / (10 ** 0) % 10
#          = 2
def d(pos: int) -> int:
    ndigits = 1
    while pos > ndigits * 9 * (10 ** (ndigits - 1)):
        pos -= ndigits * 9 * (10 ** (ndigits - 1))
        ndigits += 1
    q, r = (pos - 1) // ndigits, (pos - 1) % ndigits
    num = 10 ** (ndigits - 1) + q

    return cast(int, (num // (10 ** (ndigits - r - 1))) % 10)


def compute() -> str:
    return str(d(1) * d(10) * d(100) * d(1_000) * d(10_000) * d(100_000) * d(1_000_000))


def solve() -> str:
    return compute()
