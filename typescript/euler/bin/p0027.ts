// project euler: problem 27

/*
  n^2 + an + b,  where abs(a) < 1000 and abs(b) < 1000

  when 'n' = 0:
    '0 + 0 + b' = 'b' must be a prime number. so, 2 < 'b' < 1000.
    ('b' must not be 2 since value of the expression becomes even when 'n' is an even number)
  when 'n' = 1:
    '1 + a + b' must be a prime number. write this prime number is 'x', then 'a' = 'x' - 'b' - 1.
    abs('x' - b - 1) < 1000 and 2 < 'b' < 1000 ===> 0 < 'x' < 2000
  when 'n' is a odd number:
    'n^2 + b' is a even number. so 'a' must be a odd number.
*/

import { getPrimes, isPrimeSimple } from "../lib/primes.ts";

const countConsecutive = (a: number, b: number): number => {
  let n = 0;
  while (isPrimeSimple(n * n + a * n + b) === true) {
    n += 1;
  }

  return n;
};

export const compute = (): string => {
  const primes = getPrimes(2000);
  let maxLen = 0;
  let maxTpl: [number, number] = [0, 0];
  for (const b of primes.slice(1).filter((x) => x < 1000)) {
    for (
      const a of primes.filter((x) => Math.abs(x - b - 1) < 1000).map((x) =>
        x - b - 1
      )
    ) {
      const len = countConsecutive(a, b);
      if (len > maxLen) {
        maxLen = len;
        maxTpl = [a, b];
      }
    }
  }

  return String(maxTpl[0] * maxTpl[1]);
};

export const solve = (): string => compute();
