# project euler: problem 22

from collections.abc import Iterable
from typing import IO


def total_score(words: Iterable[str]) -> int:
    def score(word):
        return sum(ord(c) - (ord('A') - 1) for c in word)

    return sum(idx * score(word) for idx, word in enumerate(words, start=1))


def compute(fh: IO) -> str:
    keywords = [s.strip('"') for s in fh.read().split(',')]

    return str(total_score(sorted(keywords)))


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('p022_names.txt')
    result = compute(fh)
    fh.close()
    return result
