
// project euler: problem 36

import { isPalindrome, range } from "../lib/util.ts";

function isBothPalindrome(n: number): boolean {
  const n_d = n.toString();
  const n_b = n.toString(2);

  return isPalindrome(n_d) === true && isPalindrome(n_b) === true;
}

export function compute(limit: number): string {
  let acc = 0;
  for (const i of range(1, limit, 2)) {
    if (isBothPalindrome(i) === true) {
      acc += i;
    }
  }

  return String(acc);
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
