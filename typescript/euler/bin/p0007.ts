
// project euler: problem 7

import { primeGenerator } from "../lib/primes.ts";

export function compute(limit: number): string {
  let answer = 0;
  const p_gen = primeGenerator();

  for (let i = 0; i < limit; i++) {
    answer = p_gen.next().value as number;
  }

  return String(answer);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(10_001);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
