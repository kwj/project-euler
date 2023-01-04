
def prime_generator():
    prime = 2
    tbl = {}
    while True:
        if prime in tbl:
            for n in tbl[prime]:
                tbl.setdefault(prime + n, []).append(n)
            del tbl[prime]
        else:
            tbl[prime ** 2] = [prime]
            yield prime
        prime += 1
