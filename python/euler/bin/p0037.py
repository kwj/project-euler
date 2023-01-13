
# project euler: problem 37

"""
  candidate numbers: [2357][1379]*[37] (n >= 10)
                           ----------- lst
"""

from euler.lib.util import num_of_digits
from euler.lib.prime import is_prime
from time import perf_counter

def add_prefix_num(pre_lst, lst):
    return [p * (10 ** num_of_digits(n)) + n for p in pre_lst for n in lst]

def make_next_lists(lst):
    return list(filter(is_prime, add_prefix_num([1, 3, 7, 9], lst)))

def pickup_primes(lst):
    def is_truncable_prime(n):
        if n == 0:
            return False
        while n != 0:
            if is_prime(n) == False:
                return False
            n //= 10
        return True

    return list(filter(is_truncable_prime, add_prefix_num([2, 3, 5, 7], lst)))

def compute():
    result = []
    lst = [3, 7]
    while len(result) < 11:
        result += pickup_primes(lst)
        lst = make_next_lists(lst)
    assert len(result) == 11, "many candidates"

    return str(sum(result))

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
