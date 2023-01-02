
def get_primes(limit):
    '''Returns a sequence of prime numbers in a range of less than or equal to a specific value.

    >>> get_primes(100)
    [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
    '''
    primes = list(range(limit + 1))
    primes[2 * 2::2] = [0] * len(primes[2 * 2::2])
    for i in range(3, limit + 1, 2):
        if primes[i] != 0:
            primes[i * i::i] = [0] * len(primes[i * i::i])

    return [x for x in primes if x > 1]

def get_prime_tbl(limit):
    '''Returns a table for prime number determination

    >>>list(enumerate(get_prime_tbl(20)))
    [(0, False), (1, False), (2, True), (3, True), (4, False), (5, True), (6, False),
     (7, True), (8, False), (9, False), (10, False), (11, True), (12, False), (13, True),
     (14, False), (15, False), (16, False), (17, True), (18, False), (19, True), (20, False)]
    '''
    prime_tbl = [False] * 2 + [True] * (limit - 1)
    prime_tbl[2 * 2::2] = [False] * len(prime_tbl[2 * 2::2])
    for i in range(3, limit + 1, 2):
        if prime_tbl[i] == True:
            prime_tbl[i * i::i] = [False] * len(prime_tbl[i * i::i])

    return prime_tbl

def tbl_to_primes(tbl):
    primes, _ = zip(*filter(lambda tpl: tpl[1] == True, enumerate(tbl)))

    return primes
                    
