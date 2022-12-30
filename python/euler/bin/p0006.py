
# project euler: problem 6

from time import perf_counter

def compute(num):
    sum_of_square = sum(i * i for i in range(1, num + 1))
    square_of_sum = pow((1 + num) * num // 2, 2)

    return str(abs(sum_of_square - square_of_sum))

def solve():
    start = perf_counter()
    result = compute(100)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
