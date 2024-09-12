from importlib import import_module
from pathlib import Path
from time import perf_counter


def main(nlst):
    for i in nlst:
        num = int(i)
        print('[Problem {}]'.format(i))
        if (Path(__file__).parents[0] / 'bin' / 'p{:04d}.py'.format(num)).exists():
            module = import_module('euler.bin.p{:04d}'.format(num))
            start = perf_counter()
            result = module.solve()
            elapsed_time = perf_counter() - start
            print('Answer: {}'.format(result))
            print('Elapsed time: {:f} sec.\n'.format(elapsed_time))
        else:
            print('solver is not found.\n')
