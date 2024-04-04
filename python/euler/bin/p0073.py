# project euler: problem 73

# Unfortunately, the following method fails with a recursion error.
#
#     def compute(limit):
#         def dfs(left, right):
#             middle = left + right
#             if middle > limit:
#                 return 0
#             else:
#                 return dfs(left, middle) + dfs(middle, right) + 1
#
#         return str(dfs(3, 2))
#
#     > Traceback (most recent call last):
#     >   ...
#     > RecursionError: maximum recursion depth exceeded in comparison
#
#
# I tried using ohter approach but it was slow.
#
#     def compute(limit):
#         count = 0
#         work_list = [(3, 2)]
#
#         while len(work_list) > 0:
#             left_d, right_d = work_list.pop()
#             med_d = left_d + right_d
#             if med_d <= limit:
#                 work_list.append((left_d, med_d))
#                 work_list.append((med_d, right_d))
#                 count += 1
#
#         return str(count)
#
# Brute force is not interesting.
#
#     def compute(limit):
#         cnt = 0
#         for denom in range(1, limit + 1):
#             for numerator in range((denom // 3) + 1, (denom + 1) // 2):
#                 if gcd(numerator, denom) == 1:
#                     cnt += 1
#
#         return str(cnt)
#
#
# That's why I solved it with math.
#
# f(n): number of fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2
#       --> sigma{i=1, ...,n}((i-1)//2 - i//3)
# g(n): number of irreducible fractions a/b, where a < b, b <= n, 1/3 < a/b < 1/2, gcd(a,b)=1
#
#   The answer we should seek is g(12000).
#
# f(n) = sigma{k=1, ..., n}(g(n//k))
# -->
#   g(n) = sigma{k=1, ..., n}μ(k)f(n//k)      [möbius inversion formula, μ(): möbius function]
#        = sigma{k=1, ..., n}μ(k)sigma{j=1, ..., n//k}((j-1)//2 - j//3)

from math import isqrt


def mobius_tbl(limit: int) -> list[int]:
    p_tbl = list(range(limit + 1))
    for i in range(2, isqrt(limit) + 1):
        if p_tbl[i] == i:
            k = i * i
            for j in range(k, limit + 1, i):
                p_tbl[j] = i
            for j in range(k, limit + 1, k):
                p_tbl[j] = 0

    tbl = [0] * (limit + 1)
    tbl[1] = 1
    for i in range(2, limit + 1):
        if p_tbl[i] != 0:
            tbl[i] = -tbl[i // p_tbl[i]]

    return tbl


def f(x: int) -> int:
    return sum((j - 1) // 2 - j // 3 for j in range(1, x + 1))


def g(N: int) -> int:
    mb_tbl = mobius_tbl(N)
    return sum(mb_tbl[k] * f(N // k) for k in range(1, N + 1))


def compute(upper: int) -> str:
    return str(g(upper))


def solve() -> str:
    return compute(12_000)
