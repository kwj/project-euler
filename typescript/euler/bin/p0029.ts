
// project euler: problem 29

import { isqrt, lcm } from "../lib/math.ts";
import { range } from "../lib/util.ts";

function log(x: number, y: number): number {
  return Math.log(y) / Math.log(x);
}

export function compute(upper: number): string {
  const dup_tbl = Array.from(Array(upper + 1), _ => Array(upper + 1).fill(0));
  for (const x of range(2, isqrt(upper) + 1)) {
    for (const y of range(2, Math.floor(log(x, upper)) + 1)) {
      for (const z of range(1, y)) {
        const k = lcm(y, z) / y;
        for (let i = Math.max(k, 2); i <= (upper * z / y); i += k) {
          dup_tbl[x ** y][i] = 1;
        }
      }
    }
  }

  return String((upper - 1) ** 2 - dup_tbl.flat().filter((n) => n === 1).length);
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
