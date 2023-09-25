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


def powm(base: int, exp: int, modulas: int) -> int:
    result = 1
    base %= modulas
    while exp > 0:
        if exp % 2 == 1:
            result = (result * base) % modulas
        base = (base * base) % modulas
        exp //= 2

    return result


def compute() -> str:
    modulas = 10_000_000_000

    return str((28433 * powm(2, 7830457, modulas) + 1) % modulas)


def solve() -> str:
    return compute()
