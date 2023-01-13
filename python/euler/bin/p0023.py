
# project euler: problem 23

from time import perf_counter

ULIMIT = 28123

def get_abndnt_nums():
    # lookup table: sum of its proper divisors
    sd_tbl = [1] * (ULIMIT + 1)
    for i in range(2, ULIMIT + 1):
        for j in range(2 * i, ULIMIT + 1, i):
            sd_tbl[j] += i

    return list(filter(lambda x: x < sd_tbl[x], range(12, ULIMIT + 1)))

def compute():
    def is_sum_of_two_abndnts(n):
        # n = x + (n-x). x is abundant number.
        # if (n-x) is abundant number, return True.
        for x in abndnt_nums:
            if x > n // 2:
                break
            if abndnt_flag[n - x] == True:
                return True
        return False

    abndnt_nums = get_abndnt_nums()
    abndnt_flag = [False] * (ULIMIT + 1)
    for idx in abndnt_nums:
        abndnt_flag[idx] = True

    return str(sum(n for n in range(1, ULIMIT + 1) if is_sum_of_two_abndnts(n) == False))

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
