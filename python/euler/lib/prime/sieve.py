
from math import isqrt

class Sieve:
    '''Segmented Sieve of Eratosthenes
    '''
    def __init__(self, begin, end):
        self._begin = begin
        self._end = end

        self._mini_primes = []
        self._mini_tbl = []
        self._update_minitbl()
        self._make_prime_tbl()

    def _update_minitbl(self, new_size=0):
        def start_pos(n):
            offset = n - (ext_begin % n) if ext_begin % n != 0 else 0
            return max(n * n, ext_begin + offset)

        if new_size != 0 and new_size < self._end:
            return

        limit = isqrt(max(self._end, new_size)) + 1
        ext_begin = len(self._mini_tbl) if new_size != 0 else 0
        self._mini_tbl.extend(list(range(ext_begin, limit)))
        self._mini_tbl[start_pos(2)::2] = [0] * len(self._mini_tbl[start_pos(2)::2])
        for i in range(3, limit, 2):
            if self._mini_tbl[i] != 0:
                self._mini_tbl[start_pos(i)::i] = [0] * len(self._mini_tbl[start_pos(i)::i])

        self._mini_primes.extend([x for x in self._mini_tbl[ext_begin:] if x > 1])

        return

    def _make_prime_tbl(self):
        def start_pos(n):
            offset = n - (self._begin % n) if self._begin % n != 0 else 0
            return max(n * n, self._begin + offset) - self._begin

        self._prime_tbl = list(range(self._begin, self._end + 1))
        for i in self._mini_primes:
            if i * i > self._end:
                break
            self._prime_tbl[start_pos(i)::i] = [0] * len(self._prime_tbl[start_pos(i)::i])

        self._primes = [x for x in self._prime_tbl if x > 1]

        return

    def update(self, begin, end):
        if end > self._end:
            self._update_minitbl(end)

        self._begin = begin
        self._end = end
        self._make_prime_tbl()

        return

    def get_primes(self):
        return self._primes

    def is_prime(self, num):
        return self._prime_tbl[num - self._begin] > 1

def sieve(end, begin=1):
    if end < begin:
        begin, end = end, begin

    assert end > 1, 'range error'
    return Sieve(begin, end)
