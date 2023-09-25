// project euler: problem 20

import { factorialBigint, sum } from "../lib/math.ts";

export function compute(n: bigint): string {
  return String(
    sum(String(factorialBigint(n)).split("").map((x) => Number(x))),
  );
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(100n);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
