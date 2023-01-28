
// project euler: problem 12

import { numOfDivisors } from "../lib/math.ts";

export function compute(limit: number): string {
  let triangle = 1;
  let inc = 2;
  while (numOfDivisors(triangle) <= limit) {
    triangle += inc;
    inc += 1;
  }

  return String(triangle);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(500);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
