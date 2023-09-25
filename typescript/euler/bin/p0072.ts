// project euler: problem 72

/*
  See https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/
*/

import { isqrt } from "../lib/math.ts";
import { range } from "../lib/util.ts";

function sumPhi(n: number, memo?: Record<string, number>): number {
  const trunc = Math.trunc;

  const key = String(n);
  let cache: Record<string, number> = {};
  if (memo !== undefined) {
    cache = memo;
  }
  if (cache[key]) {
    return cache[key];
  }

  let v = trunc(n * (n + 1) / 2);
  for (const m of range(2, isqrt(n) + 1)) {
    v -= sumPhi(trunc(n / m), cache);
  }
  for (const d of range(1, trunc(n / (isqrt(n) + 1)) + 1)) {
    v -= (trunc(n / d) - trunc(n / (d + 1))) * sumPhi(d, cache);
  }

  return cache[key] = v;
}

export function compute(limit: number): string {
  return String(sumPhi(limit) - sumPhi(1));
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(1_000_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
