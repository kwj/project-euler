# project euler: problem 27

# n^2 + an + b,  where abs(a) < 1000 and abs(b) < 1000
#
# when 'n' = 0:
#   '0 + 0 + b' = 'b' must be a prime number. so, 2 < 'b' < 1000.
#       'b' must not be 2
#       if 'n' is an even number, the value of the expression becomes even
# when 'n' = 1:
#   '1 + a + b' must be a prime number.
#   write this prime number is 'x', then 'a' = 'x' - 'b' - 1.
#       abs('x' - b - 1) < 1000 and 2 < 'b' < 1000 ===> 0 < 'x' < 2000
# when 'n' is a odd number:
#   'n^2 + b' is a even number. so 'a' must be a odd number.

from euler.lib.prime import is_prime, primes


def count_consecutive(a: int, b: int) -> int:
    n = 0
    while is_prime(n * n + a * n + b):
        n += 1

    return n


def compute() -> str:
    p_lst = primes(2000)
    max_len = 0
    max_tpl = (0, 0)
    for b in filter(lambda x: x < 1000, p_lst[1:]):
        for a in map(lambda x: x - b - 1, filter(lambda x: abs(x - b - 1) < 1000, p_lst)):
            if (length := count_consecutive(a, b)) > max_len:
                max_len = length
                max_tpl = (a, b)

    return str(max_tpl[0] * max_tpl[1])


def solve() -> str:
    return compute()
