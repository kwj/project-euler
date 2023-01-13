
# project euler: problem 22

from euler.lib.resource import asset_file
from time import perf_counter

def total_score(words):
    def score(word):
        return sum(ord(c) - (ord('A') - 1) for c in word)

    return sum(idx * score(word) for idx, word in enumerate(words, start=1))

def compute(fh):
    keywords = [s.strip('"') for s in fh.read().split(',')]

    return str(total_score(sorted(keywords)))

def solve():
    fh = asset_file('https://projecteuler.net/project/resources/p022_names.txt')
    start = perf_counter()
    result = compute(fh)
    elapsed_time = perf_counter() - start
    fh.close()

    return (result, "{:f}".format(elapsed_time))
