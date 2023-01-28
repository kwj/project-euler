
// project euler: problem 10

import { sum } from "../lib/math.ts";
import { getPrimes } from "../lib/primes.ts";

export function compute(limit: number): string {
  return String(sum(getPrimes(limit)));
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(2_000_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
