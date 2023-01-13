
# project euler: problem 60

'''
This implementation is slow. The following is a result on Raspberry Pi 4.

$ ./solve.py 60
[Problem 60]
Answer: 26033
Elapsed time: 62.858064 sec.

It is easy to find a 5-clique, however, it needs time to confirm its sum is the smallest.

It completed about 22 seconds when I ran this program on Mac mini 2018 (Intel i5-8500).
If we use other fast programming language, we will also be able to achieve the one-minute
rule by this algorithm on Raspberry Pi 4.
'''

from euler.lib.prime import is_probably_prime, prime_generator
from itertools import combinations
import sys
from time import perf_counter

def is_pair(x, y):
    def concat_num(a, b):
        n = 10
        while b > n:
            n = n * 10
        return a * n + b

    return is_probably_prime(concat_num(x, y)) and is_probably_prime(concat_num(y, x))

def find_nbrs(prime, prime_set, limit):
    return {x for x in prime_set if x + prime < limit and is_pair(x, prime) == True}

def is_clique(lst, tbl):
    for idx in range(len(lst) - 1):
        if tbl[lst[idx]] >= set(lst[idx + 1:]):
            continue
        else:
            return False
    return True

def compute():
    # discard 2, 3 and 5
    p_gen = prime_generator()
    _ = next(p_gen); _ = next(p_gen); _ = next(p_gen)

    tbl = {3: set()}
    prime_set = [set(), {3}, {3}]    # Grouping by modulus of 3. prime_set[0] isn't used.
    answer = sys.maxsize

    while (prime := next(p_gen)) < answer - 792:    # 792 = sum([3, 7, 109, 673])
        grp = prime % 3
        tbl[prime] = (nbr_set := find_nbrs(prime, prime_set[grp], answer))
        prime_set[grp].add(prime)
        if len(nbr_set) < 4:
            continue

        for prime_grp in combinations(sorted(list(nbr_set), reverse=True), 4):
            if prime + sum(prime_grp) > answer:
                continue
            if is_clique(list(prime_grp), tbl) == True:
                answer = min(prime + sum(prime_grp), answer)

    return str(answer)

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
