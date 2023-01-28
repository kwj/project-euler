
// project euler: problem 29

import { range } from "../lib/util.ts";

function bruteForce(upper: number): number {
  const n_set = new Set();
  for (const a of range(2, upper + 1)) {
    for (const b of range(2, upper + 1)) {
      n_set.add(a ** b);
    }
  }

  return Array.from(n_set).length;
}

export function compute(upper: number): string {
  return String(bruteForce(upper));
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(100);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
