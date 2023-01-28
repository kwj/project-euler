
// project euler: problem 55

import { isPalindrome, range } from "../lib/util.ts";

export function compute(limit: number): string {
  let acc = 0;

  loop_i:
  for (let i of range(0, limit)) {
    for (const _ of range(0, 50)) {
      i += Number(String(i).split("").reverse().join(""));
      if (isPalindrome(String(i)) === true) {
        continue loop_i;
      }
    }
    acc += 1;
  }

  return String(acc);
}

export function solve(): void {
  const t0 = performance.now();
  const result = compute(10_000);
  const t1 = performance.now();
  const duration_ms = (t1 - t0).toFixed(4);

  console.log(`Answer: ${result}`);
  console.log(`Elapsed time: ${duration_ms} msec.`);

  return;
}
