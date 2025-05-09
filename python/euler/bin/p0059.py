# project euler: problem 59

import operator
import re
from collections.abc import Collection, Iterable
from itertools import product
from typing import TextIO


def decode(cipher_text: Collection[int], key: tuple[int, ...]) -> list[int]:
    text_len = len(cipher_text)
    key_len = len(key)
    key_lst = (key * (text_len // key_len + 1))[:text_len]

    return list(map(operator.xor, cipher_text, key_lst))


def calc_score(lst: Iterable[int]) -> int:
    word_list = [s.casefold() for s in re.split(r'[,. ]', ''.join(map(chr, lst)))]
    cnt = 0
    for word in ['and', 'of', 'a', 'to', 'in']:
        cnt += word_list.count(word)

    return cnt


def compute(fh: TextIO) -> str:
    cipher_text = list(map(int, fh.read().split(',')))
    key_lst = product(range(ord('a'), ord('z') + 1), repeat=3)

    max_score: tuple[int, tuple[int, ...], list[int]] = (0, (), [])
    for key in key_lst:
        plain_text = decode(cipher_text, key)
        if (score := calc_score(plain_text)) > max_score[0]:
            max_score = (score, key, plain_text)

    return str(sum(max_score[2]))


def solve() -> str:
    from euler.lib.resource import asset_file

    fh = asset_file('p059_cipher.txt')
    result = compute(fh)
    fh.close()
    return result
