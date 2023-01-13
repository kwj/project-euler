
# project euler: problem 99

'''
  log10 base^exp = exp * (log10 base)

  We will need the following file to run this program.
    - https://projecteuler.net/project/resources/p099_base_exp.txt
'''

from euler.lib.resource import asset_file
from math import log10
from time import perf_counter

def compute(fh):
    def parse_data(fh):
        return map(lambda lst: (int(lst[0]), int(lst[1])) ,[line.split(',') for line in fh.read().splitlines()])

    calc_result = map(lambda tpl: tpl[1] * log10(tpl[0]), parse_data(fh))

    return str(sorted(enumerate(calc_result), key=lambda x: x[1])[-1][0] + 1)

def solve():
    fh = asset_file('https://projecteuler.net/project/resources/p099_base_exp.txt')
    start = perf_counter()
    result = compute(fh)
    elapsed_time = perf_counter() - start
    fh.close()

    return (result, "{:f}".format(elapsed_time))
