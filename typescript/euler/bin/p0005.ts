// project euler: problem 5

import { lcm } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export function compute(upper: number): string {
  return String(lcm(range(1, upper + 1)));
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(20);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
