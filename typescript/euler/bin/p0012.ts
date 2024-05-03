// project euler: problem 12

/*
  triangle number's formula is n(n + 1)/2 and 'n' and 'n + 1' are coprime.
  Therefore, ...
    - 'n/2' and 'n+1' are coprime (when 'n' is even)
    - 'n' and '(n+1)/2' are coprime (when 'n' is odd)

  assume that f(n) returns number of divisors of 'n'.
  f(a*b) = f(a) * f(b) when 'a' and 'b' are coprime.
*/

import { numOfDivisors } from "../lib/math.ts";

export const compute = (limit: number): string => {
  let n = 1;
  while (true) {
    if (numOfDivisors(n) * numOfDivisors((n + 1) / 2) > limit) {
      break;
    }
    n += 1;
    if (numOfDivisors(n / 2) * numOfDivisors(n + 1) > limit) {
      break;
    }
    n += 1;
  }

  return String(n * (n + 1) / 2);
};

export const solve = (): string => compute(500);
