
// project euler: problem 15

import { factorial } from "../lib/math.ts";

export function compute(m: number, n: number): string {
  const trunc = Math.trunc;

  return String(trunc(trunc(factorial(m + n) / factorial(m)) / factorial(n)));
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(20, 20);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
