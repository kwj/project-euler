#!/usr/bin/env python3

import sys

import euler

# I use features which introduced in Python 3.11.
min_major_version = 3
min_minor_version = 11


def version_check() -> bool:
    if sys.version_info[0] == min_major_version and sys.version_info[1] >= min_minor_version:
        return True

    return False


if version_check() is False:
    raise RuntimeError(f'This solver requires Python {min_major_version}.{min_minor_version} or later')

args = sys.argv

if len(args) > 1 and all(str.isdecimal(x) for x in args[1:]):
    euler.main(args[1:])
else:
    print('argument error')
    print(f'  usage: {args[0]} num#1 [num#2 ...]')
    print('    num - problem number')
