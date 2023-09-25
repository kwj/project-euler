# project euler: problem 25


def compute(digits: int) -> str:
    limit = pow(10, digits - 1)
    nth = 2
    fib1, fib2 = 1, 1
    while fib2 < limit:
        nth += 1
        fib1, fib2 = fib2, fib2 + fib1

    return str(nth)


def solve() -> str:
    return compute(1_000)
