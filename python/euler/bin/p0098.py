# project euler: problem 98

from collections.abc import Iterator
from itertools import combinations, count
from math import isqrt
from typing import IO

from euler.lib.util import assoc_group_dict, num_of_digits


def select_keywords(words: list[str]) -> tuple[int, list[tuple[str, list[str]]]]:
    def make_key(word: str) -> str:
        return ''.join(sorted(list(word)))

    def make_kw_info(
        tpl_lst: Iterator[tuple[str, str]],
    ) -> tuple[int, list[tuple[str, list[str]]]]:
        work = assoc_group_dict(tpl_lst)
        del_list = []
        for k, v in work.items():
            if len(v) == 1:
                del_list.append(k)
        for k in del_list:
            del work[k]

        result = sorted(list(work.items()), key=lambda x: len(x[0]), reverse=True)
        max_digits = len(result[0][0])

        return (max_digits, result)

    return make_kw_info(map(lambda word: (make_key(word), word), words))


def make_sq_tbl(max_digits: int) -> tuple[dict[int, list[str]], dict[int, list[str]]]:
    limit = isqrt((10**max_digits) - 1)
    work = []
    for n in count(1):
        if n > limit:
            break
        sq = n * n
        work.append((num_of_digits(sq), str(sq)))

    sq_tbl = assoc_group_dict(work)
    sq_uniq_tbl = dict(
        (k, list(filter(lambda x: len(x) == len(set(x)), v))) for k, v in sq_tbl.items()
    )

    return (sq_tbl, sq_uniq_tbl)


def compute(fh: IO) -> str:
    def check_pair(w1: str, w2: str) -> int | None:
        ndigits = len(w1)
        tbl = sq_uniq_tbl if ndigits == len(set(w1)) else sq_tbl

        for sq in tbl[ndigits]:
            pair_dict = assoc_group_dict(zip(w1, sq))
            if all([len(v) == 1 for v in pair_dict.values()]) is True:
                if (w2_trans := w2.translate(str.maketrans(w1, sq))) in tbl[ndigits]:
                    w1_num = int(w1.translate(str.maketrans(w1, sq)))
                    w2_num = int(w2_trans)
                    return max(w1_num, w2_num)

        return None

    words = [s.strip('"') for s in fh.read().split(',')]
    max_digits, kw = select_keywords(words)
    sq_tbl, sq_uniq_tbl = make_sq_tbl(max_digits)

    for _, v in kw:
        for w1, w2 in combinations(v, 2):
            if (result := check_pair(w1, w2)) is not None:
                return str(result)

    assert False, 'unreachable!'


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('p098_words.txt')
    result = compute(fh)
    fh.close()
    return result
