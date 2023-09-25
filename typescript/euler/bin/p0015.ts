// project euler: problem 15

import { factorialBigint } from "../lib/math.ts";

export function compute(m: number, n: number): string {
  return String(
    factorialBigint(BigInt(m + n)) / factorialBigint(BigInt(m)) /
      factorialBigint(BigInt(n)),
  );
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
