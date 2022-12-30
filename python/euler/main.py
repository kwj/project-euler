
from functools import reduce
from importlib import import_module
from pathlib import Path
import sys

def main(nlst):
    for i in nlst:
        num = int(i)
        if (Path(__file__).parents[0] / 'bin' / 'p{:04d}.py'.format(num)).exists():
            module = import_module('euler.bin.p{:04d}'.format(num))
            result_str, et_str = module.solve()
            print('[Problem {}]'.format(i))
            print('Answer: {}'.format(result_str))
            print('Elapsed time: {} sec.\n'.format(et_str))
        else:
            print('[Problem {}]'.format(i))
            print('solver is not found.\n')
