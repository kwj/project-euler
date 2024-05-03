// project euler: problem 88

/*
  N(k) = a1 + a2 + ... + ak = a1 * a2 * ... * ak
    N(k) must be composite numbers.
  min_N(k): minimal product-sum N(k)

  when k = 2
    sum {2,2} = prod {2,2}
  when k > 2 and {a1, a2, a3, ..., ak}
    min sum = sum {1, 1, ..., 1} = k
    --> min_N(k) >= k
  when k > 2 and {a1, a2, a3, ..., ak} = {1, ..., 1, 2, k}
    for all k>2, there exists Ak = {a1, a2, ..., ak} = {1, 1, ..., 1, 2, k}, prod Ak = sum Ak = N(k) = 2k
    --> min_N(k) <= 2k

  2 <= k <= 12000
  --> k <= N(k) <= 24000

    >>> math.log2(24000)
    14.550746785383243
    N(2) = {2, 2}
    N(k) = {a1, a2, ..., an, 1, 1, ..., 1}  [k>=3,n<k]
      2 <= n <= 14  [a1, ..., an > 1]

  I'll calculate product-sum numbers from prime factors.


  example: start = [2,2] limit = 31

  sequence(terms)
  --------
  2,2
  2,2,2
  2,2,2,2
  2,2,2,3    // 2*2*2*2*2 = 32 > limit
  2,2,3      // 2*2*2*4 = 32 > limit
  2,2,4
  2,2,5
  2,2,6
  2,2,7
  2,3        // 2*2*8 = 32 > limit
  2,3,3
  2,3,4
  2,3,5
  2,4        // 2*3*6 = 36 > limit
  2,5
  2,6
  2,7
  2,8
  2,9
  2,10
  2,11
  2,12
  2,13
  2,14
  2,15
  3,3        // 2*16 = 32 > limit
  3,3,3
  3,4        // 3*3*4 = 36 > limit
  3,5
  3,6
  3,7
  3,8
  3,9
  3,10
  4,4        // 3*11 = 33 > limit
  4,5
  4,6
  4,7
  5,5        // 4*8 = 32 > limit
  5,6
  <end of search>    // 5*7 = 35 > limit and 6*6 = 36 > limit
*/

import { prod, sum } from "../lib/math.ts";

function* kvGenerator(upper: number): Generator<number[], void, unknown> {
  function makeNextTerms(lst: number[]): number[] {
    const len = lst.length;
    let tmp = lst.concat(lst[len - 1]);
    if (prod(tmp) <= upper) {
      return tmp;
    }
    tmp = lst.slice(0, len - 1).concat(lst[len - 1] + 1);
    if (prod(tmp) <= upper) {
      return tmp;
    }
    if (len === 2) {
      return [lst[0] + 1, lst[0] + 1];
    } else {
      return lst.slice(0, len - 2).concat(lst[len - 2] + 1);
    }
  }

  function makeKV(lst: number[]): number[] {
    return [prod(lst) - (sum(lst) - lst.length), prod(lst)];
  }

  let terms = [2, 2];
  while (true) {
    if (prod(terms) > upper) {
      break;
    } else {
      const result = makeKV(terms);
      terms = makeNextTerms(terms);
      yield result;
    }
  }
}

export const compute = (limit: number): string => {
  const tbl = new Map<number, number>();
  const kv_gen = kvGenerator(limit * 2);
  for (const [k, v] of kv_gen) {
    if (k <= limit && (tbl.has(k) === false || tbl.get(k)! > v)) {
      tbl.set(k, v);
    }
  }

  return String(sum([...(new Set<number>(tbl.values())).values()]));
};

export const solve = (): string => compute(12_000);
