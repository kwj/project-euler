# project euler: problem 97

#   >>> math.log((10**10 - 1) * (10**10 - 1), 2)
#   66.43856189745871
#
#   However I don't use builtin pow() on this problem because it's not fun.
#
#   Here is an information about modular exponentiation.
#     - https://en.wikipedia.org/wiki/Modular_exponentiation
#
#   Note: Similar to problem 48.


def powm(base: int, exp: int, modulo: int) -> int:
    result = 1
    base %= modulo
    while exp > 0:
        if exp % 2 == 1:
            result = (result * base) % modulo
        base = (base * base) % modulo
        exp //= 2

    return result


def compute() -> str:
    modulo = 10_000_000_000

    return str((28433 * powm(2, 7830457, modulo) + 1) % modulo)


def solve() -> str:
    return compute()
