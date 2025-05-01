# project euler: problem 42

from typing import IO

from euler.lib.util import is_triangular


def calc_scores(words: list[str]) -> list[int]:
    a_to_z = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    tbl = dict(zip(a_to_z, range(1, len(a_to_z) + 1), strict=True))

    def score(word: str) -> int:
        return sum(map(lambda c: tbl[c], list(word)))

    return list(map(score, words))


def compute(fh: IO) -> str:
    keywords = [s.strip('"') for s in fh.read().split(',')]

    return str(len(list(filter(is_triangular, calc_scores(keywords)))))


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('p042_words.txt')
    result = compute(fh)
    fh.close()
    return result
