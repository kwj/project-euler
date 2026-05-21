# project euler: problem 6


def compute(num: int) -> str:
    sum_of_squares = sum(i * i for i in range(1, num + 1))
    square_of_sum = pow((1 + num) * num // 2, 2)

    # The square of sum is equal or larger than the sum of squares.
    return str(square_of_sum - sum_of_squares)


def solve() -> str:
    return compute(100)
