// project euler: problem 45

/*
  H_{n} = T_{2n-1}
*/

import { isPentagonal } from "../lib/math.ts";

export const compute = (): string => {
  const hexNum = (n: number): number => n * (2 * n - 1);

  let d = 144;
  while (true) {
    const n = hexNum(d);
    if (isPentagonal(n) === true) {
      return String(n);
    }
    d += 1;
  }
};

export const solve = (): string => compute();
