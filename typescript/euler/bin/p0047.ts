// project euler: problem 47

import { primeFactorization } from "../lib/factor.ts";

export const compute = (nfactors: number): string => {
  let cnt = 0;

  for (let x = 1;; x++) {
    if (primeFactorization(x).length !== nfactors) {
      cnt = 0;
    } else if (cnt === nfactors - 1) {
      return String(x - (nfactors - 1));
    } else {
      cnt += 1;
    }
  }
};

export const solve = (): string => compute(4);
