# project euler: problem 43


def compute() -> str:
    lst = ['']
    for d in [1, 1, 17, 13, 11, 7, 5, 3, 2, 1]:
        lst = [x + s for x in '0123456789' for s in lst if (x not in s) and int((x + s)[:3]) % d == 0]

    return str(sum(map(int, filter(lambda s: s[0] != '0', lst))))


def solve() -> str:
    return compute()
