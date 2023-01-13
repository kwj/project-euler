
# project euler: problem 70

'''
  The answer must be a composite number because prime 'n' is not a permutation of phi(n) = n - 1.

  n = p1^k1 * p2^k2 * p3^k3 * ... * p{r}^k{n}
  -->
    phi(N) = N * (1-1/p1) * (1-1/p2) * (1-1/p3) * ... * (1-1/p{n})
      <-->
    N/phi(N) = (p1/(p1-1)) * (p2/(p2-1)) * (p3/(p3-1)) * ... * (p{n}/(p{n}-1))

  From the problem statement, 87109/phi(87109) = 87109 / 79180 = 1.1001.
  11/10 = 1.1 and 7/6 = 1.666..., so 11 <= prime numbers <= 9_999_999 / 11 = 909090.8181...

  The answer N has the following form (p_i are prime numbers)

    N = p1^k1 * p2^k2 * ... * pn^kn  (N < 10^7, n > 1, 11 <= p1 < p2 < ... < pn, k1>2 when n=1)
'''

from euler.lib.prime import get_prime_tbl, tbl_to_primes
from euler.lib.util import HeapQueue
from itertools import takewhile, dropwhile
from functools import reduce
from operator import mul
from math import isqrt
from time import perf_counter

LIMIT = 10 ** 7 - 1

class Sieve:
    def __init__(self, limit):
        self.__prime_tbl = get_prime_tbl(limit)

    def get_primes(self):
        return tbl_to_primes(self.__prime_tbl)

    def prev_prime(self, num):
        match num:
            case n if n > len(self.__prime_tbl) - 1:
                raise ValueError("too large")
            case n if n <= 2:
                raise ValueError("too small")
            case _:
                num -= 1
                while self.__prime_tbl[num] == False:
                    num -= 1
                return num

def prod(pf_lst):
    return reduce(mul, map(lambda tpl: pow(tpl[0], tpl[1]), pf_lst))

def phi(pf_lst):
    return reduce(mul, map(lambda tpl: pow(tpl[0], tpl[1] - 1) * (tpl[0] - 1), pf_lst))

def get_ratio(pf_lst):
    return prod(pf_lst) / phi(pf_lst)

def pf_generator(prime_t, tpl):
    # Note:
    #   The internal data 'pf_lst' has the following structure.
    #     [(p_n, e_n), ..., (p2, e2), (p1, e1)]
    #   In contrast, the function 'next' returns its reversed list.
    #     [(p1, e1), (p2, e2), ..., (p_n, e_n)]
    def aux(pf_lst):
        b, e = pf_lst[0]
        if (tmp := LIMIT // prod(pf_lst)) < b:
            return pf_lst
        else:
            if (prev_p := prime_t.prev_prime(tmp + 1)) > b:
                return [(prev_p, 1)] + pf_lst
            else:
                return [(b, e + 1)] + pf_lst[1:]

    pf_lst = [(tpl[1], 1), (tpl[0], 1)] if tpl[0] != tpl[1] else [(tpl[0], 2)]

    def pf_next():
        nonlocal pf_lst
        while True:
            b, e = pf_lst[0]
            if len(pf_lst) == 1 and e == 1:
                # [(p_1, 1)] ->  go to the next prime smaller than p_1
                break

            result = list(reversed(pf_lst))
            if e > 1:
                # [(p_n, e_n), ...] --> [(p_n, e_n - 1), ...]
                pf_lst[0] = (b, e - 1)
            else:
                prev_p = prime_t.prev_prime(b)
                b_x, e_x = pf_lst[1]
                if prev_p == b_x:
                    # [(p_n, 1), (p_{n-1}, e_{n-1}), ...] -> [(p_{n-1}, e_{n-1} + 1), ...]
                    pf_lst = aux([(b_x, e_x + 1)] + pf_lst[2:])
                else:
                    # [(p_n, 1), (p_{n-1}, e_{n-1}), ...] -> [(prev_prime(p_{n}), 1); (p_{n-1}, e_{n-1}; ...]
                    pf_lst = aux([(prev_p, 1)] + pf_lst[1:])

            yield result

    return pf_next

def is_perm(x, y):
    return ''.join(sorted(str(x))) == ''.join(sorted(str(y)))

def compute():
    # priority queue:
    #   (n/phi(n), prime_factors)
    # initial data for pruning:
    #   phi(87109) = 79180, 87109 = 11 * 7919
    pq = HeapQueue()
    pq.insert((87109 / 79180, [(11, 1), (7919, 1)]))

    prime_t = Sieve((LIMIT // 11) + 1)
    prime_lst = list(takewhile(lambda n: n <= isqrt(LIMIT), dropwhile(lambda n: n < 11, prime_t.get_primes())))
    for p in reversed(prime_lst):
        if get_ratio([(p, 1)]) > pq.peek()[0]:
            # pruning: end of search
            break
        pf_gen = pf_generator(prime_t, (p, prime_t.prev_prime((LIMIT // p) + 1)))
        for pf_lst in pf_gen():
            if get_ratio(pf_lst[:2]) > pq.peek()[0]:
                # pruning: skip to the next prime smaller than 'p'
                break
            if is_perm(prod(pf_lst), phi(pf_lst)) == True:
                pq.insert((get_ratio(pf_lst), pf_lst))

    return str(prod(pq.peek()[1]))

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))