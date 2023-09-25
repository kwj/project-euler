// project euler: problem 46

import { isqrt } from "../lib/math.ts";
import { isPrimeSimple } from "../lib/primes.ts";

function isTwiceSquare(n: number): boolean {
  const tmp = Math.trunc(n / 2);
  const tmp_isqrt = isqrt(tmp);

  return n % 2 === 0 && tmp_isqrt * tmp_isqrt === tmp;
}

export function compute(): string {
  // '2' is not an odd prime number
  const oddPrimes = [3, 5, 7, 11, 13, 17, 19, 23, 29, 31];

  // The odd composite numbers less than 35 have been written in the problm statement
  loop_x:
  for (let x = 35;; x += 2) {
    if (isPrimeSimple(x) === true) {
      oddPrimes.push(x);
      continue;
    }
    for (const p of oddPrimes) {
      if (isTwiceSquare(x - p) === true) {
        continue loop_x;
      }
    }
    return String(x);
  }
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
