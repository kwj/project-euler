# project euler: problem 1


def sum_of_divs_by(n: int, ulimit: int) -> int:
    upper = ulimit - 1

    return (n + (upper - (upper % n))) * (upper // n) // 2


def compute(ulimit: int) -> str:
    return str(
        sum_of_divs_by(3, ulimit)
        + sum_of_divs_by(5, ulimit)
        - sum_of_divs_by(15, ulimit)
    )


def solve() -> str:
    return compute(1000)
