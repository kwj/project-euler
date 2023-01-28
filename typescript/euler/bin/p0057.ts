
// project euler: problem 57

import { range } from "../lib/util.ts";

export function compute(n: number): string {
  let ans = 0;
  let b = 1n, c = 1n;
  for (const _ of range(0, n)) {
    [b, c] = [2n * c + b, c + b];
    if (String(b).length > String(c).length) {
      ans += 1;
    }
  }

  return String(ans);
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
