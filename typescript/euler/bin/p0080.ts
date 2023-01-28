
// project euler: problem 80

/*
  when n < 100,

    i <= 10^99 * sqrt(n) < i + 1
   -->
    i^2 <= 10^198 * n < (i + 1)^2

  'i' is the 100-digit number we want.
*/

import { isqrt, sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export function compute(limit: number, digit: number): string {
  const exp = BigInt((digit - 1) * 2);

  let acc = 0;
  for (const n of range(1, limit + 1)) {
    if (isqrt(n) * isqrt(n) === n) {
      continue;
    }
    acc += sum(String(isqrt((10n ** exp) * BigInt(n))).split("").map((x) => Number(x)));
  }

  return String(acc);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(100, 100);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
