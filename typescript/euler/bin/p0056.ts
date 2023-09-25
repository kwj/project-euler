// project euler: problem 56

import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

export function compute(): string {
  let ans = 0;
  for (const a of range(99, 0, -1)) {
    // assume that x = 10 * n
    // x^y = (10 * n)^y = 10^y * n^y, so sum_of_digits(x^y) = sum_of_digits(n^y)
    // we can skip to check multiples of ten in this problem.
    if (a % 10 === 0) {
      continue;
    }

    for (const b of range(99, 0, -1)) {
      const p_str = String(BigInt(a) ** BigInt(b));
      if (p_str.length * 9 < ans) {
        break;
      }
      ans = Math.max(sum(p_str.split("").map((x) => Number(x))), ans);
    }
  }

  return String(ans);
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
