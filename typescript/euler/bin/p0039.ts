// project euler: problem 39

/*
  assume that a <= b < c, a + b + c = p ==> a < p/3

  a^2 + b^2 = (p - a - b)^2
  => a^2 + b^2 = p^2 -2ap - 2bp + a^2 + 2ab + b^2
  => p^2 -2ap - 2bp + 2ab = 0
  => 2bp - 2ab = p^2 - 2ap
  => 2b(p - a) = p^2 - 2ap
  => b = (p^2 - 2ap) / 2(p - a)

     a  b  p  (E:even, O:odd)
   -----------
     E  E  E
     E  E  O  ==> NG (contradiction, c is even because c^2 = a^2 + b^2 is even.)
     E  O  E
     E  O  O  ==> NG (contradiction, c is odd because c^2 = a^2 + b^2 is odd.)
     O  E  E
     O  E  O  ==> NG (contradiction, c is odd because c^2 = a^2 + b^2 is odd.)
     O  O  E
     O  O  O  ==> NG (contradiction, c is even because c^2 = a^2 + b^2 is even.)

    'p' is always EVEN.
*/

import { range } from "../lib/util.ts";

export const compute = (limit: number): string => {
  const checkPair = (p: number, a: number): boolean =>
    (p * p - 2 * a * p) % (2 * (p - a)) === 0;

  const trunc = Math.trunc;
  const result: [number, number][] = [];
  for (const p of range(2, limit + 1, 2)) {
    const lst: [number, number, number][] = [];
    for (const a of range(1, trunc((p + 2) / 3))) {
      if (checkPair(p, a)) {
        const b = trunc((p * p - 2 * a * p) / (2 * (p - a)));
        lst.push([a, b, p - a - b]);
      }
    }
    if (lst.length > 0) {
      result.push([lst.length, p]);
    }
  }
  result.sort((x, y) => y[0] - x[0]);

  return String(result[0][1]);
};

export const solve = (): string => compute(1_000);
