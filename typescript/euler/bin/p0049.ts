// project euler: problem 49

import { combinations } from "combinatorics/mod.ts";
import { Sieve } from "../lib/primes.ts";

export function compute(limit: number): string {
  function isPerm(p1: number, p2: number, p3: number): boolean {
    const s1 = String(p1).split("").sort().join("");
    const s2 = String(p2).split("").sort().join("");
    const s3 = String(p3).split("").sort().join("");

    return s1 === s2 && s1 === s3;
  }

  const pt = new Sieve(1000, limit);
  for (const [i, j] of combinations(pt.getPrimes(), 2)) {
    const m = Math.trunc((i + j) / 2);
    if (pt.isPrime(m) === true && isPerm(i, m, j) === true) {
      if (i + j === 1487 + 8147) {
        continue;
      }
      return String(i) + String(m) + String(j);
    }
  }

  throw new Error("not reached");
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(9_999);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
