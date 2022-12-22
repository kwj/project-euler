#!/usr/bin/env python3

from functools import reduce
import sys
import euler

args = sys.argv

if len(args) > 1 and reduce(lambda x, y: x and str.isdecimal(y), args[1:], True):
    euler.main(args[1:])
else:
    print('argument error')
    print('  usage: {} num#1 num#2 ...'.format(args[0]))
    print('    num - problem number')
