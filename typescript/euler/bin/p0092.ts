// project euler: problem 92

/*
  9^2 = 81
  -->
        99 -> 9^2 * 2 = 162
       999 -> 9^2 * 3 = 243
      9999 -> 9^2 * 4 = 324
     99999 -> 9^2 * 5 = 405
    999999 -> 9^2 * 6 = 486
   9999999 -> 9^2 * 7 = 567

  If we know lengths of chain about the range of n <= 567,
  we can find the length of chain quickly if n > 567.

  BTW,

  This problem can be solved by combination and permutation because
  the next chain is determined by combination of numeric digit.

  Once a combination of digit numbers is determined, the total number of
  numbers represented by the combination, i.e., the number of permutations
  of multisets, can be obtained.

    n! / (k{1}! * k{2}! * ... * k{n}!)   [where n = k{1} + k{2} + ... + k{n}]

  For exapmle, we assume that a 4-digit combination with repetition is {1, 2, 2, 3}.

  They can be considered as one group since next chain of all of them
  is 18 (1^2 + 2^2 + 2^2 + 3^2). Note that the final number in this chain is 89.

  There are 12 numbers presented by the combination as following.

    1223, 1232, 1322, 2123, 2132, 2213,
    2231, 2312, 2321, 3122, 3212, 3221

  The value of 12 can be obtained from above permutations with repetitions formula:

    {num of digits}! / ({num of '1'}! * {num of '2}! * {num of '3'}!)
      = 4! / (1! * 2! * 1!)
      = 24 / 2
      = 12

  On the other hand, we assume that an another combination with repetition is {1, 2, 3, 3}.
  There are 12 numbers from the combination in the same way.

    1233, 1323, 1332, 2133, 2313, 2331,
    3123, 3132, 3213, 3231, 3312, 3321

  However, the chain from this combination {1, 2, 3, 3} arrives at 1.
  We can therefore ignore the combination.


  Note:
    Number of combinations with repetition
    https://en.wikipedia.org/wiki/Combination#Number_of_combinations_with_repetition

    Permutations of multisets
    https://en.wikipedia.org/wiki/Permutation#Permutations_of_multisets
*/

import { unzip } from "@std/collections";
import { combinationsWithReplacement } from "combinatorics/mod.ts";
import { factorial, prod, sum } from "../lib/math.ts";
import { Counter, numOfDigits } from "../lib/util.ts";

const isGroup89 = (n: number): boolean => {
  while (n !== 89 && n > 1) {
    let acc = 0;
    while (n !== 0) {
      acc += (n % 10) ** 2;
      n = Math.trunc(n / 10);
    }
    n = acc;
  }

  return n === 89;
};

export const compute = (limit: number): string => {
  if (!Number.isInteger(Math.log10(limit)) || limit === 0) {
    throw new Error(
      `this implementation works correctly only if the limit is a power of 10. (limit = ${limit})`,
    );
  }
  const ndigits = numOfDigits(limit) - 1;

  const subsetInfoLst: number[][] = [];
  for (
    const pat of combinationsWithReplacement(
      [0, 1, 4, 9, 16, 25, 36, 49, 64, 81],
      ndigits,
    )
  ) {
    if (isGroup89(sum(pat))) {
      // We need only each size of subsets, the elements of subsets themselves are not required.
      const [_, x] = unzip(Counter(pat));
      subsetInfoLst.push(x);
    }
  }

  const denominators = subsetInfoLst.map((x) =>
    prod(x.map((y) => factorial(y)))
  );
  const numerator = factorial(ndigits);

  return String(sum(denominators.map((d) => numerator / d)));
};

export const solve = (): string => compute(10_000_000);
