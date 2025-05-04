# project euler: problem 89

#   step 1:
#     IIIIIIIII     IX
#     XXXXXXXXX     XC
#     CCCCCCCCC     CM
#
#   step 2:
#     VIIII         IX
#     LXXXX         XC
#     DCCCC         CM
#
#   step 3:
#     IIIII         V
#     XXXXX         L
#     CCCCC         D
#
#   step 4:
#     IIII          IV
#     XXXX          XL
#     CCCC          CD

import re
from typing import TextIO


def replace_numbers(line: str) -> str:
    s = re.sub('IIIIIIIII|XXXXXXXXX|CCCCCCCCC', '##', line)  # step 1  IX/XC/CM
    s = re.sub('VIIII|LXXXX|DCCCC', '##', s)  # step 2  IX/XC/CM
    s = re.sub('IIIII|XXXXX|CCCCC', '#', s)  # step 3  V/L/D
    s = re.sub('IIII|XXXX|CCCC', '##', s)  # step 4  IV/XL/CD

    return s


def compute(fh: TextIO) -> str:
    acc = 0
    for line in fh.read().splitlines():
        acc += len(line) - len(replace_numbers(line))

    return str(acc)


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('p089_roman.txt')
    result = compute(fh)
    fh.close()
    return result
