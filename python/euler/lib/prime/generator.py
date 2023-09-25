from collections.abc import Generator


def prime_generator() -> Generator[int, None, None]:
    prime = 2
    tbl: dict[int, list[int]] = {}
    while True:
        if prime in tbl:
            for n in tbl[prime]:
                tbl.setdefault(prime + n, []).append(n)
            del tbl[prime]
        else:
            tbl[prime**2] = [prime]
            yield prime
        prime += 1
