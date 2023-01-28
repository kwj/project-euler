
import { unzip } from "std/collections/unzip.ts";
import { bitLength, highOrderReduce, range } from "./util.ts";

/*
  Factorial
 */
export function factorial(n: number): number {
  if (n < 0 || Number.isInteger(n) != true) {
    throw new Error("invalid argument");
  }
  if (n < 2) {
    return 1;
  }

  for (let i = n - 1; i > 1; i--) {
    n *= i;
  }

  return n;
}

export function factorialBigint(n: bigint): bigint {
  if (n < 0n) {
    throw new Error("invalid argument");
  }
  if (n < 2n) {
    return 1n;
  }

  for (let i = n - 1n; i > 1n; i--) {
    n *= i;
  }

  return n;
}

/*
  Integer Square Root
    https://github.com/mdickinson/snippets/blob/master/proofs/isqrt/src/isqrt.lean
 */

function isqrtNumber(n: number): number {
  function aux(c: number, n: number): number {
    if (c === 0) {
      return 1;
    } else {
      const k: number = Math.trunc((c - 1) / 2);
      const a: number = aux(Math.trunc(c / 2), Math.trunc(n / (2 ** (2 * k + 2))));

      return (a * (2 ** k)) + Math.trunc(Math.trunc(n / (2 ** (k + 2))) / a);
    }
  }

  if (n === 0) {
    return 0;
  } else {
    const a = aux(Math.trunc((bitLength(n) - 1) / 2), n);
    if (n < a * a) {
      return a - 1;
    } else {
      return a;
    }
  }
}

function isqrtBigint(n: bigint): bigint {
  function aux(c: bigint, n: bigint): bigint {
    if (c === 0n) {
      return 1n;
    } else {
      const k = (c - 1n) / 2n;
      const a = aux(c / 2n, n / (2n ** (2n * k + 2n)));

      return (a * (2n ** k)) + (n / (2n ** (k + 2n))) / a;
    }
  }

  if (n === 0n) {
    return 0n;
  } else {
    const a = aux((BigInt(bitLength(n)) - 1n) / 2n, n);
    if (n < a * a) {
      return a - 1n;
    } else {
      return a;
    }
  }
}

export function isqrt<T extends number | bigint>(n: T): T {
  if (typeof n === "number") {
    return isqrtNumber(n as number) as T;
  } else {
    return isqrtBigint(n as bigint) as T;
  }
}

/*
  Modular exponentiation
    https://en.wikipedia.org/wiki/Modular_exponentiation
 */
export function modPow(base: bigint, exp: bigint, mod: bigint): bigint {
  let result = 1n;
  base = base % mod;
  while (exp > 0) {
    if ((exp & 1n) === 1n) {
      result = (result * base) % mod;
    }
    base = (base * base) % mod;
    exp = exp >> 1n;
  }

  return result;
}

/*
  Prime factorization and more
 */
export function factorize(n: number): [number, number][] {
  if (n < 1) {
    throw new Error("parameter is too small");
  } else if (n === 1) {
    return [[1, 1]];
  }

  const result: [number, number][] = [];
  for (const b of [2, 3, 5]) {
    let e = 0;
    while (n % b === 0) {
      e += 1;
      n = Math.trunc(n / b);
    }
    if (e != 0) {
      result.push([b, e]);
    }
  }

  // 7, 11, 13, 17, 19, 23, 29, 31, (37, ...)
  const diff = [4, 2, 4, 2, 4, 6, 2, 6];
  let b = 7;
  let idx = 0;
  const limit = isqrt(n);

  while (b <= limit) {
    let e = 0;
    while (n % b === 0) {
      e += 1;
      n = Math.trunc(n / b);
    }
    if (e != 0) {
      result.push([b, e]);
    }
    b += diff[idx];
    idx = (idx + 1) % 8;
  }

  if (n != 1) {
    result.push([n, 1]);
  }

  return result;
}

export function pflstToNumber(pf_lst: [number, number][]): number {
  return pf_lst.map((lst) => lst[0] ** lst[1])
               .reduce((acc, cur) => acc * cur, 1);
}

export function pflstToDivisors(pf_lst: [number, number][]): number[] {
  let div_lst = [1];
  for (const tpl of pf_lst) {
    let acc_lst: number[] = [];
    for (const m of range(1, tpl[1] + 1).map((e) => tpl[0] ** e)) {
      acc_lst = acc_lst.concat(div_lst.map((x) => x * m));
    }
    div_lst = div_lst.concat(acc_lst);
  }

  if (div_lst[1] === 1) {
    return [1];
  } else {
    return div_lst.sort((a, b) => {return a - b;});
  }
}

export function divisors(n: number): number[] {
  return pflstToDivisors(factorize(n));
}

export function numOfDivisors(n: number): number {
  const [_, e]: [number[], number[]] = unzip(factorize(n));
  return e.map((x) => x + 1)
          .reduce((acc, cur) => acc * cur, 1);
}

/*
  funtions for array
 */
// max
export function max(...lst: number[] | number[][]): number {
  if (Array.isArray(lst[0]) === true) {
    return highOrderReduce<number>(Math.max, lst[0] as number[]);
  } else {
    return highOrderReduce<number>(Math.max, lst as number[]);
  }
}

// min
export function min(...lst: number[] | number[][]): number {
  if (Array.isArray(lst[0]) === true) {
    return highOrderReduce<number>(Math.min, lst[0] as number[]);
  } else {
    return highOrderReduce<number>(Math.min, lst as number[]);
  }
}

// gcd
export function gcd(...lst: number[] | number[][]): number {
  function _gcd(a: number, b: number): number {
    if (b === 0) {
      return a;
    } else {
      return _gcd(b, a % b);
    }
  }

  if (Array.isArray(lst[0]) === true) {
    return highOrderReduce<number>(_gcd, lst[0] as number[]);
  } else {
    return highOrderReduce<number>(_gcd, lst as number[]);
  }
}

// lcm
export function lcm(...lst: number[] | number[][]): number {
  function _lcm(a: number, b: number): number {
    return a / gcd(a, b) * b;
  }

  if (Array.isArray(lst[0]) === true) {
    return highOrderReduce<number>(_lcm, lst[0] as number[]);
  } else {
    return highOrderReduce<number>(_lcm, lst as number[]);
  }
}

// sum
export function sum(...lst: number[] | number[][]): number {
  function _add(a: number, b: number): number {
    return a + b;
  }

  if (Array.isArray(lst[0]) === true) {
    return highOrderReduce<number>(_add, lst[0] as number[]);
  } else {
    return highOrderReduce<number>(_add, lst as number[]);
  }
}

// sum for Bigint
export function sumBigint(...lst: bigint[] | bigint[][]): bigint {
  function _add(a: bigint, b: bigint): bigint {
    return a + b;
  }

  if (Array.isArray(lst[0]) === true) {
    return highOrderReduce<bigint>(_add, lst[0] as bigint[]);
  } else {
    return highOrderReduce<bigint>(_add, lst as bigint[]);
  }
}

// prod
export function prod(...lst: number[] | number[][]): number {
  function _prod(a: number, b: number): number {
    return a * b;
  }

  if (Array.isArray(lst[0]) === true) {
    return highOrderReduce<number>(_prod, lst[0] as number[]);
  } else {
    return highOrderReduce<number>(_prod, lst as number[]);
  }
}

/*
  polygonal number
 */
export function isTriangle(n: number): boolean {
  const tmp = 8 * n + 1;
  const tmp_sqrt = isqrt(tmp);

  return tmp_sqrt * tmp_sqrt === tmp && tmp_sqrt % 2 === 1;
}

export function isSquare(n: number): boolean {
  const n_sqrt = isqrt(n);

  return n_sqrt * n_sqrt === n;
}

export function isPentagonal(n: number): boolean {
  const tmp = 24 * n + 1;
  const tmp_sqrt = isqrt(tmp);

  return tmp_sqrt * tmp_sqrt === tmp && tmp_sqrt % 6 === 5;
}

export function isHexagonal(n: number): boolean {
  const tmp = 8 * n + 1;
  const tmp_sqrt = isqrt(tmp);

  return tmp_sqrt * tmp_sqrt === tmp && tmp_sqrt % 4 === 3;
}
