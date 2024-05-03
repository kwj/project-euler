// project euler: problem 70

/*
  The answer must be a composite number because prime 'n' is not a permutation of phi(n) = n - 1.

  n = p1^k1 * p2^k2 * p3^k3 * ... * p{r}^k{n}
  -->
    phi(N) = N * (1-1/p1) * (1-1/p2) * (1-1/p3) * ... * (1-1/p{n})
      <-->
    N/phi(N) = (p1/(p1-1)) * (p2/(p2-1)) * (p3/(p3-1)) * ... * (p{n}/(p{n}-1))

  From the problem statement, 87109/phi(87109) = 87109 / 79180 = 1.1001.
  11/10 = 1.1 and 7/6 = 1.666..., so 11 <= prime numbers <= 9_999_999 / 11 = 909090.8181...

  The answer N has the following form (p_i are prime numbers)

    N = p1^k1 * p2^k2 * ... * pn^kn  (N < 10^7, n > 1, 11 <= p1 < p2 < ... < pn, k1>2 when n=1)
*/

import { ascend, BinaryHeap } from "std/data_structures/mod.ts";
import { dropWhile } from "std/collections/drop_while.ts";
import { takeWhile } from "std/collections/take_while.ts";
import { isqrt } from "../lib/math.ts";
import { getPrimeTbl, primeTblToPrimes } from "../lib/primes.ts";

const LIMIT = 10 ** 7 - 1;

class Sieve {
  prime_tbl: boolean[] = [];

  constructor(limit: number) {
    this.prime_tbl = getPrimeTbl(limit);
  }

  getPrimes(): number[] {
    return primeTblToPrimes(this.prime_tbl);
  }

  prev_prime(n: number): number {
    if (n > this.prime_tbl.length - 1) {
      throw new RangeError("too large");
    } else if (n <= 2) {
      throw new RangeError("too small");
    } else {
      n -= 1;
      while (this.prime_tbl[n] === false) {
        n -= 1;
      }
      return n;
    }
  }
}

const prod = (pfLst: number[][]): number =>
  pfLst.map((x) => x[0] ** x[1]).reduce((acc, cur) => acc * cur, 1);

const phi = (pfLst: number[][]): number =>
  pfLst.map((x) => x[0] ** (x[1] - 1) * (x[0] - 1))
    .reduce((acc, cur) => acc * cur, 1);

const getRatio = (pfLst: number[][]): number => prod(pfLst) / phi(pfLst);

function* pfGenerator(
  prime_t: Sieve,
  tpl: [number, number],
): Generator<[number, number][], void, unknown> {
  // Note:
  //   The internal data 'pf_lst' has the following structure.
  //     [[p_n, e_n], ..., [p2, e2], [p1, e1]]
  //   In contrast, the function 'next' returns its reversed list.
  //     [[p1, e1], [p2, e2], ..., [p_n, e_n]]

  const aux = (pfLst: [number, number][]): [number, number][] => {
    const [b, e] = pfLst[0];
    const tmp = Math.trunc(LIMIT / prod(pfLst));
    if (tmp < b) {
      return pfLst;
    } else {
      const prev_p = prime_t.prev_prime(tmp + 1);
      if (prev_p > b) {
        return [[prev_p, 1]].concat(pfLst) as [number, number][];
      } else {
        return [[b, e + 1]].concat(pfLst.slice(1)) as [number, number][];
      }
    }
  };

  let pfLst: [number, number][] = (tpl[0] !== tpl[1])
    ? [[tpl[1], 1], [tpl[0], 1]]
    : [[tpl[0], 2]];
  while (true) {
    const [b, e] = pfLst[0];
    if (pfLst.length === 1 && e === 1) {
      // [[p_1, 1]] ->  go to the next prime smaller than p_1
      break;
    }

    const result = pfLst.slice().reverse();
    if (e > 1) {
      // [[p_n, e_n], ...] --> [[p_n, e_n - 1], ...]
      pfLst[0] = [b, e - 1];
    } else {
      const prev_p = prime_t.prev_prime(b);
      const [b_x, e_x] = pfLst[1];
      if (prev_p === b_x) {
        // [[p_n, 1], [p_{n-1}, e_{n-1}], ...] -> [[p_{n-1}, e_{n-1} + 1], ...]
        pfLst = aux(
          [[b_x, e_x + 1]].concat(pfLst.slice(2)) as [number, number][],
        );
      } else {
        // [[p_n, 1], [p_{n-1}, e_{n-1}], ...] -> [[prev_prime(p_{n}), 1]; [p_{n-1}, e_{n-1}], ...]
        pfLst = aux([[prev_p, 1]].concat(pfLst.slice(1)) as [number, number][]);
      }
    }

    yield result;
  }
}

const isPerm = (p1: number, p2: number): boolean => {
  const s1 = String(p1).split("").sort().join("");
  const s2 = String(p2).split("").sort().join("");

  return s1 === s2;
};

export const compute = (): string => {
  // priority queue:
  //   (n/phi(n), prime_factors)
  // initial data for pruning:
  //   phi(87109) = 79180, 87109 = 11 * 7919
  const pq = new BinaryHeap<[number, number[][]]>((a, b) => ascend(a[0], b[0]));
  pq.push([87109 / 79180, [[11, 1], [7919, 1]]]);

  const prime_t = new Sieve(Math.trunc(LIMIT / 11) + 1);
  const primeLst = takeWhile(
    dropWhile(prime_t.getPrimes(), (x) => x < 11),
    (y) => y <= isqrt(LIMIT),
  );
  for (const p of primeLst.reverse()) {
    if (getRatio([[p, 1]]) > pq.peek()![0]) {
      // pruning: end of search
      break;
    }
    const pf_gen = pfGenerator(prime_t, [
      p,
      prime_t.prev_prime(Math.trunc(LIMIT / p) + 1),
    ]);
    for (const pfLst of pf_gen) {
      if (getRatio(pfLst.slice(0, 2)) > pq.peek()![0]) {
        // pruning: skip to the next prime smaller than 'p'
        break;
      }
      if (isPerm(prod(pfLst), phi(pfLst)) === true) {
        pq.push([getRatio(pfLst), pfLst]);
      }
    }
  }

  return String(prod(pq.peek()![1]));
};

export const solve = (): string => compute();
