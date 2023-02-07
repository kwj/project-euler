
// project euler: problem 17

import { range } from "../lib/util.ts";

const words = new Map<number, number>([
  [1, "one".length],
  [2, "two".length],
  [3, "three".length],
  [4, "four".length],
  [5, "five".length],
  [6, "six".length],
  [7, "seven".length],
  [8, "eight".length],
  [9, "nine".length],
  [10, "ten".length],
  [11, "eleven".length],
  [12, "twelve".length],
  [13, "thirteen".length],
  [14, "fourteen".length],
  [15, "fifteen".length],
  [16, "sixteen".length],
  [17, "seventeen".length],
  [18, "eighteen".length],
  [19, "nineteen".length],
  [20, "twenty".length],
  [30, "thirty".length],
  [40, "forty".length],
  [50, "fifty".length],
  [60, "sixty".length],
  [70, "seventy".length],
  [80, "eighty".length],
  [90, "ninety".length],
  [0, 0], // Data when remainder is 0
]);

export function compute(limit: number): string {
  const trunc = Math.trunc;

  let acc = 0;
  for (const n of range(1, limit + 1)) {
    if (n === 1000) {
      acc += 11;
    } else if (n < 20) {
      acc += words.get(n)!;
    } else if (n < 100) {
      acc += words.get(n - (n % 10))! + words.get(n % 10)!;
    } else if (n % 100 === 0) {
      /* "xxx" hundred -> "xxx".length + 7 */
      acc += words.get(trunc(n / 100))! + 7;
    } else if (n % 100 < 20) {
      /* "xxx" hundred and ... -> "xxx".length + 7 + 3 + ... */
      acc += words.get(trunc(n / 100))! + 7 + 3 + words.get(n % 100)!;
    } else {
      /* "xxx" hundred and ... -> "xxx".length + 7 + 3 + ... */
      acc += words.get(trunc(n / 100))! + 7 + 3 + words.get((n % 100) - (n % 10))! + words.get(n % 10)!;
    }
  }

  return String(acc);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(1000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
