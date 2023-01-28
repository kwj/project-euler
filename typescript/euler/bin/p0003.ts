
// project euler: problem 3

import { factorize } from "../lib/math.ts";

export function compute(n: number): string {
  return String(factorize(n).slice(-1)[0][0]);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(600_851_475_143);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
