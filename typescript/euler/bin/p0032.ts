// project euler: problem 32

/*
  m * n = mn (multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital)

  - numbers of digits of multiplicand/multiplier must be 4 or less.
  - if number of digits of multiplicand is 4, number of digits of multiplier is 1.
  - if number of digits of multiplicand is 3, number of digits of multiplier is 2.

  multiplicand/multiplier/product : 4-digits/1-digit/4-digits or 3-digits/2-digits/4-digits
*/

import { sum } from "../lib/math.ts";
import { isPandigitalNZ, range } from "../lib/util.ts";

function makeCands(): [number, number][] {
  const acc: [number, number][] = [];

  // 4-digits/1-digit/4-digits
  for (const m1 of range(1_000, 10_000)) {
    for (const m2 of range(2, 10)) {
      if (m1 * m2 >= 10_000) {
        break;
      }
      acc.push([m1 * (10 ** 5) + m2 * (10 ** 4) + m1 * m2, m1 * m2]);
    }
  }

  // 3-digits/2-digits/4-digits
  for (const m1 of range(100, 1_000)) {
    for (const m2 of range(10, 100)) {
      if (m1 * m2 >= 10_000) {
        break;
      }
      acc.push([m1 * (10 ** 6) + m2 * (10 ** 4) + m1 * m2, m1 * m2]);
    }
  }

  return acc;
}

export function compute(): string {
  const n_set = new Set();
  for (const [n, prod] of makeCands()) {
    if (isPandigitalNZ(n) === true) {
      n_set.add(prod);
    }
  }

  return String(sum(Array.from(n_set) as number[]));
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
