
# project euler: problem 89

'''
  step 1:
    IIIIIIIII     IX
    XXXXXXXXX     XC
    CCCCCCCCC     CM

  step 2:
    VIIII         IX
    LXXXX         XC
    DCCCC         CM

  step 3:
    IIIII         V
    XXXXX         L
    CCCCC         D

  step 4:
    IIII          IV
    XXXX          XL
    CCCC          CD

  We will need the following files to run this program.
    - https://projecteuler.net/project/resources/p089_roman.txt
'''

from euler.lib.resource import asset_file
import re
from time import perf_counter

def replace_numbers(line):
    s = re.sub('IIIIIIIII|XXXXXXXXX|CCCCCCCCC', '##', line)    # step 1  IX/XC/CM
    s = re.sub('VIIII|LXXXX|DCCCC', '##', s)                   # step 2  IX/XC/CM
    s = re.sub('IIIII|XXXXX|CCCCC', '#', s)                    # step 3  V/L/D
    s = re.sub('IIII|XXXX|CCCC', '##', s)                      # step 4  IV/XL/CD

    return s

def compute(fn, fh):
    acc = 0
    for line in fh.read().splitlines():
        acc += len(line) - len(replace_numbers(line))

    return str(acc)

def solve():
    fh = asset_file('https://projecteuler.net/project/resources/p089_roman.txt')
    start = perf_counter()
    result = compute(min, fh)
    elapsed_time = perf_counter() - start
    fh.close()

    return (result, "{:f}".format(elapsed_time))
