# project euler: problem 85

#   nCr = n! / ((n-r)! * r!)
#
#       1  2       n-1  n
#     +--+--+-- ... --+--+
#    1|  |  |   ...   |  |
#     +--+--+-- ... --+--+
#    2|  |  |   ...   |  |
#     +--+--+-- ... --+--+ num of horizontal lines = m + 1
#    3|  |  |   ...   |  |
#     +--+--+-- ... --+--+
#     ....................
#     +--+--+-- ... --+--+
#    m|  |  |   ...   |  |
#     +--+--+-- ... --+--+
#       num of vertical lines = n + 1
#
#   (m+1)C2 * (n+1)C2 = m(m+1)/2 * n(n+1)/2 (\approx) 2_000_000
#   --> m(m+1)*n(n+1) (\approx) 8_000_000

from itertools import count
from math import isqrt

from euler.lib.util import HeapQueue


def get_diff(m: int, target: int) -> tuple[int, int] | None:
    def lhs(m: int, n: int) -> int:
        return m * (m + 1) * n * (n + 1)

    n = isqrt(target // (m * (m + 1))) - 1
    while lhs(m, n) < target:
        n += 1

    if m >= n:
        return None

    if abs(target - lhs(m, n - 1)) < abs(target - lhs(m, n)):
        return abs(target - lhs(m, n - 1)), n - 1
    else:
        return abs(target - lhs(m, n)), n


def compute(target: int) -> str:
    pq = HeapQueue()
    new_target = target * 4
    for m in count(1):
        if (tpl := get_diff(m, new_target)) is None:
            break
        pq.insert((tpl[0], m * tpl[1]))

    return str(pq.peek()[1])


def solve() -> str:
    return compute(2_000_000)
