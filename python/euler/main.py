
from functools import reduce
from importlib import import_module
from pathlib import Path
import sys

def main(nlst):
    for i in nlst:
        num = int(i)
        if (Path(__file__).parents[0] / 'bin' / 'p{:04d}.py'.format(num)).exists():
            module = import_module('euler.bin.p{:04d}'.format(num))
            result, etstr = module.solve()
            print('[Problem {}]'.format(i))
            print('Answer: {}'.format(result))
            print('Elapsed time: {} sec.\n'.format(etstr))
        else:
            print('[Problem {}]'.format(i))
            print('solver is not found.\n')
