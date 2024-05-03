// project euler: problem 30

/*
  (1) 10 ** (n-1) <= n-digits number < 10 ** n
  (2) assume that x is sum of the fifth power of each digit on n-digits number
        n * (1**5) <= x <= n * (9**5) = n * 54049

     when n=6:
       6 * 54049 = 324294
     when n=7
       7 * 54049 = 378343 < 10 ** (7-1) = 1000000 (minimum 7-digits number)
       --> contradiction

  It's clear that 'x' is not a single digit number.
  We need to search 'x' in the follwing range:
    10 <= 'x' <= 354294 = 6 * (9 ** 5)

  We have two approaches to solve this problem. One is to search from left hand side,
  and the other is to search from right hand side.

  1) search from LHS

    # The following function is a little slower than search_from_rhs().
    def search_from_lhs():
        limit = 354_294
        memo_tbl = [0, 1, 32, 243, 1024, 3125, 7776, 16807, 32768, 59049] + [0] * (limit + 1 - 10)
        acc = 0
        for n in range(10, limit + 1):
            memo_tbl[n] = memo_tbl[n // 10] + memo_tbl[n % 10]
            if n == memo_tbl[n]:
                acc += n

        return acc

  2) search from RHS
    We search from combinations of numbers.
*/

import { combinationsWithReplacement } from "combinatorics/mod.ts";
import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

const searchFromRhs = (): number => {
  const toDigitLst = (n: number): number[] => {
    const aux = (n: number): number[] => {
      if (n >= 10) {
        return aux(Math.trunc(n / 10)).concat(n % 10);
      } else {
        return [n];
      }
    };

    return aux(n).sort();
  };

  const powTbl = [0, 1, 32, 243, 1024, 3125, 7776, 16807, 32768, 59049];
  let acc = 0;
  for (const ndigits of range(2, 7)) {
    for (
      const lst of combinationsWithReplacement(
        range(0, 10),
        ndigits,
      ) as Generator<number[]>
    ) {
      const n = sum(lst.map((x) => powTbl[x]));
      if (toDigitLst(n).toString() === lst.toString()) {
        acc += n;
      }
    }
  }

  return acc;
};

export const compute = (): string => String(searchFromRhs());

export const solve = (): string => compute();
