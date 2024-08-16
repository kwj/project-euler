// project euler: problem 44

/*
  P(d) = P(k) - P(j) <==> d(3d-1) = k(3k-1) - j(3j-1) = (k-j)(3(k+j)-1)
*/

import { pfactorsToDivisors, primeFactors } from "../lib/factor.ts";
import { isPentagonal } from "../lib/math.ts";

// getDivisors(n) returns divisors of n(3n-1) which meet the following requirements:
//  - They are less than 'n'.
//  - They are congruent to 'n' modulo 3.
// note: 'n' and '3n-1' are co-prime numbers.
const getDivisors = (n: number): number[] =>
  pfactorsToDivisors(primeFactors(n).concat(primeFactors(3 * n - 1)))
    .filter((x) => x < n && x % 3 === n % 3);

// d(3d-1) = (k-j)(3(k+j)-1)
//   lhs: d(3d-1)
//   rhs: (k-j) * (3(k+j)-1) = r1 * r2 [r1=k-j, r2=3(k+j)-1]
//      0 < (k-j) < d, d % 3 == (k-j) % 3
export const compute = (): string => {
  const pent = (n: number): number => trunc((n * (3 * n - 1)) / 2);

  const trunc = Math.trunc;
  let d = 4;

  while (true) {
    const lhs = d * (3 * d - 1);
    for (const r1 of getDivisors(d)) {
      const r2 = trunc(lhs / r1);
      if (r2 % 3 === 2) {
        // tmp = k+j
        const tmp = trunc((r2 + 1) / 3);
        if ((r1 + tmp) % 2 === 0) {
          const k = trunc((r1 + tmp) / 2);
          const j = k - r1;
          if (isPentagonal(pent(k) + pent(j))) {
            return String(trunc(lhs / 2));
          }
        }
      }
    }
    d += 1;
  }
};

export const solve = (): string => compute();
