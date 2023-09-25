// project euler: problem 44

/*
  P(d) = P(k) - P(j) <==> d(3d-1) = k(3k-1) - j(3j-1) = (k-j)(3(k+j)-1)
*/

import { factorize, isPentagonal, pflstToDivisors } from "../lib/math.ts";

// getDivisors(n) returns divisors of n(3n-1) which meet the following requirements:
//  - They are less than 'n'.
//  - They are congruent to 'n' modulo 3.
// note: 'n' and '3n-1' are co-prime numbers.
function getDivisors(n: number): number[] {
  const divisors = pflstToDivisors([...factorize(n), ...factorize(3 * n - 1)]);

  return divisors.filter((x) => x < n && x % 3 === n % 3);
}

// d(3d-1) = (k-j)(3(k+j)-1)
//   lhs: d(3d-1)
//   rhs: (k-j) * (3(k+j)-1) = r1 * r2 [r1=k-j, r2=3(k+j)-1]
//      0 < (k-j) < d, d % 3 == (k-j) % 3
export function compute(): string {
  function pent(n: number): number {
    return trunc((n * (3 * n - 1)) / 2);
  }

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
          if (isPentagonal(pent(k) + pent(j)) === true) {
            return String(trunc(lhs / 2));
          }
        }
      }
    }
    d += 1;
  }
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute();
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
