
// project euler: problem 51

/*
  the smallest prime which, by replacing part of the number with same digit,
  is part of an eight prime value family

  -> eight numbers out of '0' to '9' are used for replacement

  1) the last digit is not eligible for replacement
    It make some even numbers after replacement.

  2) the number of digits of the prime numbers is greater than number of digits to be replaced
    the reason is since 1).

  3) the number of digits that can be replaced is only a multiples of 3
    if 'n' is not multiples of 3, some replaced numbers will contain multiples of 3.

      number  digits  sum  'mod 3'    [n>0]
      ---------------------------------------------
      0       n       0    0
      1       n       n    n mod 3
      2       n       2n   2n mod 3
      3       n       3n   3n mod 3 = 0
      4       n       4n   4n mod 3 = n mod 3
      5       n       5n   5n mod 3 = 2n mod 3
      6       n       6n   6n mod 3 = 0
      7       n       7n   7n mod 3 = n mod 3
      8       n       8n   8n mod 3 = 2n mod 3
      9       n       9n   9n mod 3 = 0

  I want to use prime number tables to speed up and simplify. For the
  moment, I assume that prime numbers less than one million. The reason
  is that I want to only consider the case of replacing three digits.

  4) There are at least same three numbers other than last digit.
*/

import { unzip } from "std/collections/unzip.ts";
import { combinations } from "combinatorics/mod.ts";
import { isPrimeSimple } from "../lib/primes.ts";
import { range } from "../lib/util.ts";

/*
  pattern:
    (num_of_digits_to_replace, [d1, ..., dn])
       1: digit to replace
       0: digit not to replace
    note: d1: Least Significant Digit (LSD)
          dn: Most Significant Digit (MSD)

  # on python
  >>> make_patterns(4)
  [(3, [0, 1, 1, 1])]
  >>> make_patterns(5)
  [(3, [0, 1, 1, 1, 0]), (3, [0, 1, 1, 0, 1]), (3, [0, 1, 0, 1, 1]), (3, [0, 0, 1, 1, 1])]
  >>> make_patterns(6)
  [(3, [0, 1, 1, 1, 0, 0]), (3, [0, 1, 1, 0, 1, 0]), (3, [0, 1, 1, 0, 0, 1]),
   (3, [0, 1, 0, 1, 1, 0]), (3, [0, 1, 0, 1, 0, 1]), (3, [0, 1, 0, 0, 1, 1]),
   (3, [0, 0, 1, 1, 1, 0]), (3, [0, 0, 1, 1, 0, 1]), (3, [0, 0, 1, 0, 1, 1]),
   (3, [0, 0, 0, 1, 1, 1])]
  >>> make_patterns(7)
  [(3, [0, 1, 1, 1, 0, 0, 0]), (3, [0, 1, 1, 0, 1, 0, 0]), (3, [0, 1, 1, 0, 0, 1, 0]),
   (3, [0, 1, 1, 0, 0, 0, 1]), (3, [0, 1, 0, 1, 1, 0, 0]), (3, [0, 1, 0, 1, 0, 1, 0]),
   (3, [0, 1, 0, 1, 0, 0, 1]), (3, [0, 1, 0, 0, 1, 1, 0]), (3, [0, 1, 0, 0, 1, 0, 1]),
   (3, [0, 1, 0, 0, 0, 1, 1]), (3, [0, 0, 1, 1, 1, 0, 0]), (3, [0, 0, 1, 1, 0, 1, 0]),
   (3, [0, 0, 1, 1, 0, 0, 1]), (3, [0, 0, 1, 0, 1, 1, 0]), (3, [0, 0, 1, 0, 1, 0, 1]),
   (3, [0, 0, 1, 0, 0, 1, 1]), (3, [0, 0, 0, 1, 1, 1, 0]), (3, [0, 0, 0, 1, 1, 0, 1]),
   (3, [0, 0, 0, 1, 0, 1, 1]), (3, [0, 0, 0, 0, 1, 1, 1]), (6, [0, 1, 1, 1, 1, 1, 1])]
*/
function makePatterns(ndigits: number): [number, number[]][] {
  function selectPos(): [number, number[]][] {
    const result: [number, number[]][] = [];
    const positions = range(1, ndigits);
    for (const n of range(3, ndigits, 3)) {
      for (const pat of combinations(positions, n)) {
        result.push([n, pat]);
      }
      // result.concat([...combinations(positions, n)].map((x) => [n, x]));
    }

    return result;
  }

  function flipBits(posLst: [number, number[]]): [number, number[]] {
    const result: number[] = Array(ndigits);
    result.fill(0);
    for (const idx of posLst[1]) {
      result[idx] = 1;
    }

    return [posLst[0], result];
  }

  return selectPos().map((x) => flipBits(x));
}

// assemble_num(231, [0, 1, 1, 1, 0, 0], 7)
//   --> 237771
function assembleNum(i: number, pat: number[], r: number): number {
  const lst: number[] = [];
  for (const flag of pat) {
    if (flag === 0) {
      lst.push(i % 10);
      i = Math.trunc(i / 10);
    } else {
      lst.push(r);
    }
  }

  return lst.reverse().reduce((x, y) => 10 * x + y);
}

function isProbable(i: number, pat: number[], currentMin: number): boolean {
  if (pat.at(-1) === 1) {
    return assembleNum(i, pat, 1) < currentMin;
  } else {
    return assembleNum(i, pat, 0) < currentMin;
  }
}

// check all cases
function findPrime(i: number, pat: number[]): number | undefined {
  // If MSB is target digit to replace, '0' is not applicable.
  const start = pat.at(-1) === 1 ? 1 : 0;

  const lst: number[] = [];
  for (const r of range(start, 10)) {
    const tmp = assembleNum(i, pat, r);
    if (isPrimeSimple(tmp) === true) {
      lst.push(tmp);
    }
  }

  // if there are eight primes by this pattern, return the smallest one.
  return (lst.length === 8) ? lst[0] : undefined;
}

export function compute(): string {
  let ndigits = 4;
  while (true) {
    let ans = 10 ** ndigits;
    const patTpls = makePatterns(ndigits);

    // n: number of digits to replace (3, 6, ...)
    for (const n of range(3, ndigits, 3)) {
      // 'i' must be odd number
      for (const i of range(10 ** (ndigits - n - 1) + 1, 10 ** (ndigits - n), 2)) {
        const [_, patLst] = unzip(patTpls.filter((x) => x[0] === n));
        for (const pat of patLst) {
          if (isProbable(i, pat, ans) === true) {
            const tmp = findPrime(i, pat);
            if (tmp !== undefined && tmp < ans) {
              ans = tmp;
            }
          }
        }
      }
    }

    if (ans !== 10 ** ndigits) {
      return String(ans);
    }
    ndigits += 1;
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
