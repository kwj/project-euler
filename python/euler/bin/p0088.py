# project euler: problem 88

#   N(k) = a1 + a2 + ... + ak = a1 * a2 * ... * ak
#     N(k) must be composite numbers.
#   min_N(k): minimal product-sum N(k)
#
#   when k = 2
#     sum {2,2} = prod {2,2}
#   when k > 2 and {a1, a2, a3, ..., ak}
#     min sum = sum {1, 1, ..., 1} = k
#     --> min_N(k) >= k
#   when k > 2 and {a1, a2, a3, ..., ak} = {1, ..., 1, 2, k}
#     for all k>2, there exists Ak = {a1, a2, ..., ak} = {1, 1, ..., 1, 2, k}, and
#     prod Ak = sum Ak = N(k) = 2k
#     --> min_N(k) <= 2k
#
#   2 <= k <= 12000
#   --> k <= N(k) <= 24000
#
#     >>> math.log2(24000)
#     14.550746785383243
#     N(2) = {2, 2}
#     N(k) = {a1, a2, ..., an, 1, 1, ..., 1}  [k>=3,n<k]
#       2 <= n <= 14  [a1, ..., an > 1]
#
#   I'll calculate product-sum numbers from prime factors.
#
#
#   example: start = [2,2] limit = 31
#
#   sequence(terms)
#   --------
#   2,2
#   2,2,2
#   2,2,2,2
#   2,2,2,3    // 2*2*2*2*2 = 32 > limit
#   2,2,3      // 2*2*2*4 = 32 > limit
#   2,2,4
#   2,2,5
#   2,2,6
#   2,2,7
#   2,3        // 2*2*8 = 32 > limit
#   2,3,3
#   2,3,4
#   2,3,5
#   2,4        // 2*3*6 = 36 > limit
#   2,5
#   2,6
#   2,7
#   2,8
#   2,9
#   2,10
#   2,11
#   2,12
#   2,13
#   2,14
#   2,15
#   3,3        // 2*16 = 32 > limit
#   3,3,3
#   3,4        // 3*3*4 = 36 > limit
#   3,5
#   3,6
#   3,7
#   3,8
#   3,9
#   3,10
#   4,4        // 3*11 = 33 > limit
#   4,5
#   4,6
#   4,7
#   5,5        // 4*8 = 32 > limit
#   5,6
#   <end of search>    // 5*7 = 35 > limit and 6*6 = 36 > limit

from collections.abc import Generator
from math import prod


# ganerate pairs of key and value
#   N(key) = value
# example:
#   terms: [2, 2] -> (2, prod([2,2])) = (2, 4)
#   terms: [2, 3, 3] -> (13, prod([2,3,3])) = (13, 18)
#     13 = len([2, 3, 3, 1, ..., 1])
#        = prod([2,3,3]) - sum(map(pred, [2,3,3]))
#        = prod([2,3,3]) - (sum[2,3,3] - len([2,3,3]))
#        = 18 - (8 - 3)
#     prod([2, 3, 3, 1, ..., 1]) = sum([2, 3, 3, 1, ..., 1]) = 18
#
def kv_generator(upper: int) -> Generator[tuple[int, int], None, None]:
    def make_next_terms(lst: list[int]) -> list[int]:
        if prod(lst + [lst[-1]]) <= upper:
            # [a, b, .., f] -> [a, b, ..., f, f]
            return lst + [lst[-1]]
        elif prod(lst[:-1] + [lst[-1] + 1]) <= upper:
            # [a, b, ..., f] -> [a, b, ..., f+1]
            return lst[:-1] + [lst[-1] + 1]
        else:
            if len(lst) == 2:
                # [a, b] -> [a+1, a+1]
                return [lst[0] + 1, lst[0] + 1]
            else:
                # [a, b, ..., e, f, g] -> [a, b, ..., e, f+1]
                return lst[:-2] + [lst[-2] + 1]

    def make_kv(lst: list[int]) -> tuple[int, int]:
        return (prod(lst) - (sum(lst) - len(lst)), prod(lst))

    terms = [2, 2]
    while True:
        if prod(terms) > upper:
            break
        else:
            result = make_kv(terms)
            terms = make_next_terms(terms)
            yield result


def compute(limit: int) -> str:
    tbl: dict[int, int] = dict()
    for k, v in kv_generator(limit * 2):
        if k <= limit and (k not in tbl or tbl[k] > v):
            tbl[k] = v

    return str(sum(set(tbl.values())))


def solve() -> str:
    return compute(12_000)
