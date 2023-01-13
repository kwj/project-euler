
# project euler: problem 43

from time import perf_counter

def compute():
    lst = ['']
    for d in [1, 1, 17, 13, 11, 7, 5, 3, 2, 1]:
        lst = [x + s for x in '0123456789' for s in lst if (x not in s) and int((x + s)[:3]) % d == 0]

    return str(sum(map(int, lst)))

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
