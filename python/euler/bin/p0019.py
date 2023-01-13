
# project euler: problem 19

from itertools import accumulate
from time import perf_counter

def compute():
    common_year = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    leap_year = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

    # days per month (Jan 1901 - Nov 2000)
    days = (((common_year * 3) + leap_year) * 25)[:-1]

    # Jan 1, 1900 was Monday and assume this day is the first day. (Monday is '1 mod 7 = 1')
    # And then, the year 1900 was common year = 365 days.
    # --> Jan 1, 1901 was Tuesday since (1 + 365) mod 7 = 2.
    #     Feb 1, 1901 was Firday since ((1 + 365) + 31) mod 7 = 5.
    #     ... and so on
    cnt = len([x for x in accumulate([1 + 365] + days) if x % 7 == 0])

    return str(cnt)

def solve():
    start = perf_counter()
    result = compute()
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
