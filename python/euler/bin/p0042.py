
# project euler: problem 42

from euler.lib.resource import asset_file
from euler.lib.util import is_triangular
from time import perf_counter

def calc_scores(words):
    a_to_z = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    tbl = dict(zip(a_to_z, range(1, len(a_to_z) + 1)))
    def score(word):
        return sum(map(lambda c: tbl[c], list(word)))

    return list(map(score, words))

def compute(fh):
    keywords = [s.strip('"') for s in fh.read().split(',')]

    return str(len(list(filter(is_triangular, calc_scores(keywords)))))

def solve():
    fh = asset_file('https://projecteuler.net/project/resources/p042_words.txt')
    start = perf_counter()
    result = compute(fh)
    elapsed_time = perf_counter() - start
    fh.close()

    return (result, "{:f}".format(elapsed_time))
