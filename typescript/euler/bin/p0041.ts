// project euler: problem 39

/*
  (sum_{1}_{9} n) mod 3 = 45 mod 3 = 0  --> 9 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{8} n) mod 3 = 36 mod 3 = 0  --> 8 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{7} n) mod 3 = 28 mod 3 = 1
  (sum_{1}_{6} n) mod 3 = 21 mod 3 = 0  --> 6 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{5} n) mod 3 = 15 mod 3 = 0  --> 5 digits pandigital number is a multiple of 3, not a prime.
  (sum_{1}_{4} n) mod 3 = 10 mod 3 = 1

  2143 is a 4-digit pandigital and is also prime.
*/

import { permutations } from "combinatorics/mod.ts";
import { isPrimeSimple } from "../lib/primes.ts";
import { isPandigitalNZ, range } from "../lib/util.ts";

// This implementation depends on the permutation lists are emitted in lexicographic ordering
// according to the order of the input *iterable*.
export function compute(): string {
  for (const digits of [range(7, 0, -1), range(4, 0, -1)]) {
    for (const nLst of permutations(digits)) {
      const n = nLst.reduce((x, y) => 10 * x + y);
      if (isPandigitalNZ(n) && isPrimeSimple(n)) {
        return String(n);
      }
    }
  }

  throw new Error("Not reached");
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
