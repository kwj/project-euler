// project euler: problem 85

/*
  nCr = n! / ((n-r)! * r!)

      1  2       n-1  n
    +--+--+-- ... --+--+
   1|  |  |   ...   |  |
    +--+--+-- ... --+--+
   2|  |  |   ...   |  |
    +--+--+-- ... --+--+ num of horizontal lines = m + 1
   3|  |  |   ...   |  |
    +--+--+-- ... --+--+
    ....................
    +--+--+-- ... --+--+
   m|  |  |   ...   |  |
    +--+--+-- ... --+--+
      num of vertical lines = n + 1

  (m+1)C2 * (n+1)C2 = m(m+1)/2 * n(n+1)/2 (\approx) 2_000_000
  --> m(m+1)*n(n+1) (\approx) 8_000_000
*/

import { ascend, BinaryHeap } from "@std/data-structures";
import { isqrt } from "../lib/math.ts";

const getDiff = (m: number, target: number): [number, number] | undefined => {
  const abs = Math.abs;

  const lhs = (m: number, n: number): number => m * (m + 1) * n * (n + 1);

  let n = isqrt(Math.trunc(target / (m * (m + 1)))) - 1;
  while (lhs(m, n) < target) {
    n += 1;
  }

  if (m >= n) {
    return undefined;
  }
  const r1 = abs(target - lhs(m, n - 1));
  const r2 = abs(target - lhs(m, n));
  if (r1 < r2) {
    return [r1, n - 1];
  } else {
    return [r2, n];
  }
};

export const compute = (target: number): string => {
  const pq = new BinaryHeap<[number, number]>((a, b) => ascend(a[0], b[0]));
  const new_target = target * 4;
  let m = 0;
  while (true) {
    m += 1;
    const result = getDiff(m, new_target);
    if (result === undefined) {
      break;
    }
    pq.push([result[0], m * result[1]]);
  }

  return String(pq.peek()![1]);
};

export const solve = (): string => compute(2_000_000);
