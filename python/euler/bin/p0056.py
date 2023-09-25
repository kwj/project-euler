# project euler: problem 56


def compute() -> str:
    answer = 0
    for a in range(99, 0, -1):
        # assume that x = 10 * n
        # x^y = (10 * n)^y = 10^y * n^y, so sum_of_digits(x^y) = sum_of_digits(n^y)
        # we can skip to check multiples of ten in this problem.
        if a % 10 == 0:
            continue
        for b in range(99, 0, -1):
            p = a**b
            if len(p_str := str(p)) * 9 < answer:
                break
            answer = max(sum(map(int, list(p_str))), answer)

    return str(answer)


def solve() -> str:
    return compute()
