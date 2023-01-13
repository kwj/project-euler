
# project euler: problem 95

from time import perf_counter

def compute(limit):
    def check_loop(pos):
        stop = pos
        min_value = pos
        cnt = 1
        while (pos := sd_tbl[pos]) != stop:
            cnt += 1
            if pos < min_value:
                min_value = pos
        return cnt, min_value

    # lookup table: sum of divisors
    sd_tbl = [1] * (limit + 1)
    for i in range(2, limit + 1):
        for j in range(2 * i, limit + 1, i):
            sd_tbl[j] += i

    # chain table
    chain_tbl = [0] * (limit + 1)
    chain_tbl[1] = 1

    answer = 0
    max_chains = 0

    for idx in range(2, limit + 1):
        # already checked
        if chain_tbl[idx] != 0:
            continue

        flag = idx
        chain_tbl[idx] = flag
        while True:
            next_idx = sd_tbl[idx]

            # outside the range
            if next_idx > limit:
                break

            # found a loop
            if chain_tbl[next_idx] == flag:
                # perfect number
                if next_idx == idx:
                    break
                # amicable pair
                if sd_tbl[next_idx] == idx:
                    break
                # amicable chain
                chains, min_value = check_loop(next_idx)
                if chains > max_chains:
                    max_chains = chains
                    answer = min_value

            # arrive at a known chain
            if chain_tbl[next_idx] != 0:
                break

            chain_tbl[next_idx] = flag
            idx = next_idx

    return str(answer)

def solve():
    start = perf_counter()
    result = compute(1_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
