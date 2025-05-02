# project euler: problem 95

from collections.abc import Iterable

from euler.lib.util import aliquot_sum_tbl


def compute(limit: int) -> str:
    def update_chain_tbl(indices: Iterable[int], v: int):
        for i in indices:
            chain_tbl[i] = v

    next_tbl = aliquot_sum_tbl(limit)
    chain_tbl = [0] * (limit + 1)
    max_length = 0

    for n in range(2, limit + 1):
        if chain_tbl[n] != 0:
            continue

        pos = n
        chain = []
        while chain_tbl[pos] == 0:
            chain.append(pos)
            pos = next_tbl[pos]
            if not (1 <= pos <= limit) or pos in chain:
                break

        if not (1 <= pos <= limit) or chain_tbl[pos] != 0:
            update_chain_tbl(chain, -1)
            continue

        i = chain.index(pos)
        length = len(chain) - i + 1
        update_chain_tbl(chain[:i], -1)
        update_chain_tbl(chain[i:], length)
        max_length = max(max_length, length)

    return str(chain_tbl.index(max_length))


def solve() -> str:
    return compute(1_000_000)
