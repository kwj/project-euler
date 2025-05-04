from collections.abc import Iterable
from importlib import import_module
from pathlib import Path
from time import perf_counter


def main(nlst: Iterable[str]) -> None:
    for i in nlst:
        num = int(i)
        print(f'[Problem {i}]')
        if (Path(__file__).parents[0] / 'bin' / f'p{num:04d}.py').exists():
            module = import_module(f'euler.bin.p{num:04d}')
            start = perf_counter()
            result = module.solve()
            elapsed_time = perf_counter() - start
            print(f'Answer: {result}')
            print(f'Elapsed time: {elapsed_time:f} sec.\n')
        else:
            print('solver is not found.\n')
