# project euler: problem 31


def compute(coins: list[int], target: int) -> str:
    tbl = [0] * (target + 1)
    tbl[0] = 1

    for c in coins:
        for i in range(c, target + 1):
            tbl[i] += tbl[i - c]

    return str(tbl[target])


def solve() -> str:
    return compute([1, 2, 5, 10, 20, 50, 100, 200], 200)
