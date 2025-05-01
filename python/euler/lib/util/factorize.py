from functools import reduce
from itertools import repeat, starmap
from math import isqrt

from ..prime import primes


def factorize(n: int) -> list[tuple[int, int]]:
    # special case
    if n < 1:
        raise AssertionError('argument is less than or equal to 0')
    elif n == 1:
        return [(1, 1)]

    result = []
    for b in [2, 3, 5]:
        e = 0
        while n % b == 0:
            e += 1
            n //= b
        if e != 0:
            result.append((b, e))

    # 7, 11, 13, 17, 19, 23, 29, 31, (37, ...)
    diff = [4, 2, 4, 2, 4, 6, 2, 6]
    b = 7
    idx = 0
    limit = isqrt(n)

    while b <= limit:
        e = 0
        while n % b == 0:
            e += 1
            n //= b
        if e != 0:
            result.append((b, e))
        b += diff[idx]
        idx = (idx + 1) % 8

    if n != 1:
        result.append((n, 1))

    return result


def pfactors_to_divisors(pf_lst: list[tuple[int, int]]) -> list[int]:
    lst = [1]
    for b, e in pf_lst:
        acc_lst = []
        for m in map(pow, repeat(b), range(1, e + 1)):
            acc_lst += list(map(lambda x: x * m, lst))
        lst += acc_lst

    if lst[1] == 1:
        return [1]
    else:
        return sorted(lst)


def pfactors_to_num(pf_lst: list[tuple[int, int]]) -> int:
    return reduce(lambda x, y: x * y, starmap(pow, pf_lst))


def divisors(num: int) -> list[int]:
    return pfactors_to_divisors(factorize(num))


def num_of_divisors(num: int) -> int:
    _, e_iter = zip(*factorize(num), strict=True)

    return reduce(lambda x, y: x * y, map(lambda x: x + 1, e_iter))


# divisor function
# https://en.wikipedia.org/wiki/Divisor_function
def sigma_tbl(z: int, upper: int) -> list[int]:
    """Note: This function returns a list which size is 'upper + 1'."""
    p_lst = primes(upper)
    result = [1] * (upper + 1)

    for p in p_lst:
        q = p
        x = 0
        while q <= upper:
            x += q**z
            result[q] += x
            q *= p

    for p in p_lst:
        q = p
        while q <= upper:
            for n in range(2, upper // q + 1):
                if result[n] == 1 or n % p == 0:
                    continue
                result[q * n] = result[q] * result[n]
            q *= p

    result[0] = 0

    return result


def aliquot_sum_tbl(upper: int) -> list[int]:
    """Note: This function returns a list which size is 'upper + 1'."""
    tbl = sigma_tbl(1, upper)
    for i in range(1, upper + 1):
        tbl[i] -= i

    return tbl
