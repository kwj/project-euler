
from ..prime import get_primes
from math import isqrt
from itertools import repeat, starmap
from functools import reduce

def factorize(n):
    # special case
    if n < 1:
        assert False
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

def pfactors_to_divisors(pf_lst):
    lst = [1]
    for b, e in pf_lst:
        acc_lst = []
        for m in map(pow, repeat(b), range(1, e+1)):
            acc_lst += list(map(lambda x: x * m, lst))
        lst += acc_lst

    if lst[1] == 1:
        return [1]
    else:
        return sorted(lst)

def pfactors_to_num(pf_lst):
    return reduce(lambda x, y: x * y, starmap(pow, pf_lst))

def divisors(num):
    return pfactors_to_divisors(factorize(num))

def num_of_divisors(num):
    _, e_iter = zip(*factorize(num))

    return reduce(lambda x, y: x * y, map(lambda x: x + 1, e_iter))

# divisor function
# https://en.wikipedia.org/wiki/Divisor_function
def get_sigma_tbl(z, upper):
    p_lst = get_primes(upper)
    result = [1] * (upper + 1)

    for p in p_lst:
        q = p
        x = 0
        while q <= upper:
            x += q ** z
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

    return result
