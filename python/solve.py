#!/usr/bin/env python3

from functools import reduce
import sys
import euler

# I use features which introduced in Python 3.10.
min_major_version = 3
min_minor_version = 10

def version_check():
    if sys.version_info.major < 3:
        return False
    if sys.version_info.major == min_major_version and sys.version_info.minor < min_minor_version:
        return False

    return True

if version_check() == False:
    raise RuntimeError('This solver requires Python {}.{} or later'.format(min_major_version, min_minor_version))


args = sys.argv

if len(args) > 1 and reduce(lambda x, y: x and str.isdecimal(y), args[1:], True):
    euler.main(args[1:])
else:
    print('argument error')
    print('  usage: {} num#1 [num#2 ...]'.format(args[0]))
    print('    num - problem number')
