
# project euler: problem 69

'''
  I didn't know Euler's Totient function, phi(n).
  So I wrote this solution by referring Wikipedia.

  https://en.wikipedia.org/wiki/Euler%27s_totient_function

    n/phi(n) = n / n(1-1/p{1})(1-1/p{2})...(1-1/p{r})
             = p{1}p{2}...p{r} / (p{1}-1)(p{2}-1)...(p{r}-1)
                    (p{i} is prime number)

  the above show that value of n/phi(n) depends on the prime factors of 'n'.

  generally, 1 < m/(m-1) and m/(m-1) > n/(n-1) [m<n].

  so I'll find the maximum 'k' which satisfies follwing condition.

    p{1} * p{2} * ... * p{k-1} * p{k} <= 1_000_000
       [p{i} is prime number: 2, 3, 5, 7, ...]

  the answer 'n' is p{1} * p{2} * ... * p{k-1} * p{k}.
'''

from euler.lib.prime import prime_generator
from time import perf_counter

def compute(limit):
    p_gen = prime_generator()
    acc = 1
    while acc <= limit:
        next_p = next(p_gen)
        if acc * next_p > limit:
            break
        acc *= next_p

    return str(acc)

def solve():
    start = perf_counter()
    result = compute(1_000_000)
    elapsed_time = perf_counter() - start

    return (result, "{:f}".format(elapsed_time))
