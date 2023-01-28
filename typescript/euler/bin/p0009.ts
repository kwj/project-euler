
// project euler: problem 9

/*
  a = k(m^2 - n^2), b = k * 2mn, c = k(m^2 + n^2)  [m>n>0, gcd(m,n)=1, m+n is odd]

  abc = k^3 * (m^4 - n^4) * 2mn
  a + b + c = k * 2m(m+n) = 1000

    -> 'k' and 'm' are divisors to 500 (= 1000/2).
       'm+n' is a divisor to 500/m.
       m(m+n) <= 500 --> m <= isqrt(500), m+n <= 500/m
*/

import { isqrt } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export function compute(perim: number): string {
  const trunc = Math.trunc;

  for (const m of range(2, isqrt(trunc(perim / 2)) + 1)) {
    if (trunc(perim / 2) % m !== 0) {
      continue;
    }

    let x = m + 1 + (m % 2);
    while (x < 2 * m && x <= trunc(trunc(perim / 2) / m)) {
      if (trunc(trunc(perim / 2) / 2) % x === 0) {
        const k = trunc(trunc(trunc(perim / 2) / m) / x);
        const n = x - m;

        return String(k ** 3 * (m ** 4 - n ** 4) * 2 * m * n);
      }
      x += 2;
    }
  }

  // not found (NOT REACHED when perimeter = 1000)
  return String(0);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(1_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
