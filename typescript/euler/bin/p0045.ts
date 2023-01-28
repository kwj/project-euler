
// project euler: problem 45

/*
  H_{n} = T_{2n-1}
*/

import { isPentagonal } from "../lib/math.ts";

export function compute(): string {
  function hexNum(n: number): number {
    return n * (2 * n - 1);
  }

  let d = 144;
  while (true) {
    const n = hexNum(d);
    if (isPentagonal(n) === true) {
      return String(n);
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
