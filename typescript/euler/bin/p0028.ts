// project euler: problem 28

/*
  21 22 23 24 25
  20  7  8  9 10
  19  6  1  2 11
  18  5  4  3 12
  17 16 15 14 13
         |  |  |  |
      (n=0, 1, 2, 3, ...)

  the upper right number is:
    1    [n=0]
    (2n+1)**2    [n=>1]

  so, the sum of numbers in the four corners is:
    (2n+1)**2 + ((2n+1)**2 - 2n) + ((2n+1)**2 - 4n) + ((2n+1)**2 - 6n)
      = 16n**2 + 4n + 4   [n>=1]

  Answer: 1 + sum_{n=1}^{(1001-1)/2} (16n**2 + 4n + 4)
*/

import { range } from "../lib/util.ts";

export function compute(len: number): string {
  let result = 1;
  for (const n of range(1, Math.trunc((len - 1) / 2) + 1)) {
    result += 16 * n * n + 4 * n + 4;
  }
  return String(result);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(1_001);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
