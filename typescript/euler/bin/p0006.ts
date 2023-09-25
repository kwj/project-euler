// project euler: problem 6

import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export function compute(upper: number): string {
  const sequence = range(1, upper + 1);

  return String(Math.abs(sum(sequence.map((n) => n * n)) - sum(sequence) ** 2));
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
