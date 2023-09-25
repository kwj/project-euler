// project euler: problem 97

import { modPow } from "../lib/math.ts";

export function compute(): string {
  const modulas = 10_000_000_000n;

  return String((28433n * modPow(2n, 7830457n, modulas) + 1n) % modulas);
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
