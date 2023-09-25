// project euler: problem 75

/*
  Pythagorean triple

    a = k * (m^2 - n^2), b = k * 2mn, c = k * (m^2 + n^2)
      where m > n > 0, gcd(m, n) = 1

    perimeter L = k * (2m^2 + 2mn)
                = k * 2m(m + n)

    2m(m + n) = L/k
      -->
    2m^2 < 2m(m + n) = L/k
      <-->
    m^2 < L/2k

    'm' is maximized when k=1
      max(m) < sqrt(L/2)
*/

import { gcd, isqrt } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export function compute(L: number): string {
  const limit = isqrt(Math.trunc(L / 2));
  const counter: number[] = new Array(L + 1).fill(0);

  for (const m of range(2, limit + 1)) {
    for (const n of range(1 + (m % 2), m, 2)) {
      if (gcd(m, n) === 1) {
        const perimeter = 2 * m * (m + n);
        if (perimeter > L) {
          break;
        }
        for (const i of range(perimeter, L + 1, perimeter)) {
          counter[i] += 1;
        }
      }
    }
  }

  return String(counter.filter((x) => x === 1).length);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(1_500_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
