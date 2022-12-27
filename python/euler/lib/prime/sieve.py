
class Sieve:
    '''Prime number flag/Least prime factors/Mobius function tables

    Compute above sequences in a range of less than or equal to a specific value.
    Note: The first element is an invalid value.

      public attributes:
        prime_tbl[]: bool
          [False, False, True, True, False, True, False, True, False, ...]
             *      1      2     3     4      5     6      7     8
        minfactor_tbl[]: int
          [-1, 1, 2, 3, 2, 5, 2, 7, 2, ...]
            *  1  2  3  4  5  6  7  8
        mobius_tbl[]: int
          [1, 1, -1, -1, 0, -1, 1, -1, 0, ...]
           *  1   2   3  4   5  6   7  8

      public methods:
        get_primes()
        is_prime()
        factorize()
        divisors()

        >>> pt = sieve.sieve(100)
        >>> pt.get_primes()
        [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
        >>> pt.is_prime(87)
        False
        >>> pt.factorize(84)
        [(2, 2), (3, 1), (7, 1)]
        >>> pt.divisors(84)
        [1, 2, 3, 4, 6, 7, 12, 14, 21, 28, 42, 84]
    '''
    def __init__(self, num):
        self.prime_tbl = [True] * (num + 1)
        self.minfactor_tbl = [-1] * (num + 1)
        self.mobius_tbl = [1] * (num + 1)

        self.prime_tbl[0] = False
        self.prime_tbl[1] = False
        self.minfactor_tbl[1] = 1

        for i in range(2, num + 1):
            if self.prime_tbl[i] == False:
                continue
            self.minfactor_tbl[i] = i
            self.mobius_tbl[i] = -1

            # Not 'i * i' for start value since caliculate mobius function
            for j in range(i * 2, num + 1, i):
                self.prime_tbl[j] = False
                if self.minfactor_tbl[j] == -1:
                    self.minfactor_tbl[j] = i
                if (j // i) % i == 0:
                    self.mobius_tbl[j] = 0
                else:
                    self.mobius_tbl[j] = -self.mobius_tbl[j]

    def is_prime(self, num):
        return self.prime_tbl[num]

    def get_primes(self, ulimit=None):
        if ulimit == 2:
            return [2]
        elif ulimit is None:
            ulimit = len(self.prime_tbl) - 1

        result = [2]
        idx = 3
        while idx <= ulimit:
            if self.prime_tbl[idx] == True:
                result.append(idx)
            idx += 2

        return result

    def factorize(self, num):
        result = []
        while num > 1:
            base = self.minfactor_tbl[num]
            exp = 0
            while self.minfactor_tbl[num] == base:
                num //= base
                exp += 1

            result.append((base,exp))

        return result

    def divisors(self, num):
        result = [1]
        for base, exp in self.factorize(num):
            r_size = len(result)
            for i in range(r_size):
                v = 1
                for _ in range(exp):
                    v *= base
                    result.append(result[i] * v)

        return sorted(result)

def sieve(num):
    return Sieve(num)
