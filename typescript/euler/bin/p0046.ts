// project euler: problem 46

import { isqrt } from "../lib/math.ts";
import { isPrimeSimple } from "../lib/primes.ts";

const isTwiceSquare = (n: number): boolean => {
  const tmp = Math.trunc(n / 2);
  const tmp_isqrt = isqrt(tmp);

  return n % 2 === 0 && tmp_isqrt * tmp_isqrt === tmp;
};

export const compute = (): string => {
  // '2' is not an odd prime number
  const oddPrimes = [3, 5, 7, 11, 13, 17, 19, 23, 29, 31];

  // The odd composite numbers less than 35 have been written in the problm statement
  loop_x:
  for (let x = 35;; x += 2) {
    if (isPrimeSimple(x) === true) {
      oddPrimes.push(x);
      continue;
    }
    for (const p of oddPrimes) {
      if (isTwiceSquare(x - p) === true) {
        continue loop_x;
      }
    }
    return String(x);
  }
};

export const solve = (): string => compute();
