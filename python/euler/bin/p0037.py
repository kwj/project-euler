# project euler: problem 37

# candidate numbers: [2357][1379]*[37] (n >= 10)
#                          ----------- lst

from euler.lib.prime import is_prime
from euler.lib.util import num_of_digits


def add_prefix_num(pre_lst: list[int], lst: list[int]) -> list[int]:
    return [p * (10 ** num_of_digits(n)) + n for p in pre_lst for n in lst]


def make_next_lists(lst: list[int]) -> list[int]:
    return list(filter(is_prime, add_prefix_num([1, 3, 7, 9], lst)))


def pickup_primes(lst: list[int]) -> list[int]:
    def is_truncable_prime(n: int) -> bool:
        if n == 0:
            return False
        while n != 0:
            if not is_prime(n):
                return False
            n //= 10
        return True

    return list(filter(is_truncable_prime, add_prefix_num([2, 3, 5, 7], lst)))


def compute() -> str:
    result: list[int] = []
    lst = [3, 7]
    while len(result) < 11:
        result += pickup_primes(lst)
        lst = make_next_lists(lst)
    assert len(result) == 11, 'many candidates'

    return str(sum(result))


def solve() -> str:
    return compute()
