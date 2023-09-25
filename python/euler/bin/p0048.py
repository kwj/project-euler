# project euler: problem 48


def compute(exp: int) -> str:
    modulus = 10**10

    return format(
        sum(pow(x, x, modulus) for x in range(1, exp + 1) if x % 10 != 0) % modulus,
        '010',
    )


def solve() -> str:
    return compute(1_000)
