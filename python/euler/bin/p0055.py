# project euler: problem 55


def compute() -> str:
    cnt = 0
    for n in range(10_000):
        i = n
        for _ in range(50):
            i += int(str(i)[::-1])
            s = str(i)
            if s == s[::-1]:
                break
        else:
            cnt += 1

    return str(cnt)


def solve() -> str:
    return compute()
