# project euler: problem 6


def compute(num: int) -> str:
    sum_of_square = sum(i * i for i in range(1, num + 1))
    square_of_sum = pow((1 + num) * num // 2, 2)

    return str(abs(sum_of_square - square_of_sum))


def solve() -> str:
    return compute(100)
