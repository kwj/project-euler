import { unzip } from "@std/collections";
import { bitLength, range } from "./util.ts";

/* Factorial */
export const factorial = <T extends number | bigint>(n: T): T => {
  if (typeof n === "number") {
    return factorialNumber(n as number) as T;
  } else {
    return factorialBigint(n as bigint) as T;
  }
};

const factorialNumber = (n: number): number => {
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
};

const factorialBigint = (n: bigint): bigint => {
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
};

/* Integer Square Root */
export const isqrt = <T extends number | bigint>(n: T): T => {
  if (typeof n === "number") {
    return isqrtNumber(n as number) as T;
  } else {
    return isqrtBigint(n as bigint) as T;
  }
};

const isqrtNumber = (n: number): number => {
  return Math.trunc(Math.sqrt(n));
};

// https://github.com/mdickinson/snippets/blob/master/proofs/isqrt/src/isqrt.lean
const isqrtBigint = (n: bigint): bigint => {
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
};

/*
  Modular exponentiation
    https://en.wikipedia.org/wiki/Modular_exponentiation
 */
export const modPow = <T extends number | bigint>(
  base: T,
  exp: T,
  mod: T,
): T => {
  if (typeof base === "number") {
    return modPowNumber(base as number, exp as number, mod as number) as T;
  } else {
    return modPowBigint(base as bigint, exp as bigint, mod as bigint) as T;
  }
};

const modPowNumber = (base: number, exp: number, mod: number): number => {
  let result = 1;
  base = base % mod;
  while (exp > 0) {
    if ((exp & 1) === 1) {
      result = (result * base) % mod;
    }
    base = (base * base) % mod;
    exp = exp >> 1;
  }

  return result;
};

const modPowBigint = (base: bigint, exp: bigint, mod: bigint): bigint => {
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
};

/*
  Prime factorization and more
 */
export const factorize = (n: number): [number, number][] => {
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
};

export const pflstToNumber = (pf_lst: [number, number][]): number => {
  return pf_lst.map((lst) => lst[0] ** lst[1]).reduce(
    (acc, cur) => acc * cur,
    1,
  );
};

export const pflstToDivisors = (pf_lst: [number, number][]): number[] => {
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
    return div_lst.sort((a, b) => {
      return a - b;
    });
  }
};

export const divisors = (n: number): number[] => {
  return pflstToDivisors(factorize(n));
};

export const numOfDivisors = (n: number): number => {
  const [_, e]: [number[], number[]] = unzip(factorize(n));
  return e.map((x) => x + 1)
    .reduce((acc, cur) => acc * cur, 1);
};

/*
  funtions for array
 */
// max
export const maxLst = <T extends number | bigint>(lst: T[]): T => {
  const _max_number = (a: number, b: number): number => a < b ? b : a;
  const _max_bigint = (a: bigint, b: bigint): bigint => a < b ? b : a;

  if (typeof lst[0] === "number") {
    return (lst as number[]).reduce(_max_number) as T;
  } else {
    return (lst as bigint[]).reduce(_max_bigint) as T;
  }
};

// min
export const minLst = <T extends number | bigint>(lst: T[]): T => {
  const _min_number = (a: number, b: number): number => a < b ? a : b;
  const _min_bigint = (a: bigint, b: bigint): bigint => a < b ? a : b;

  if (typeof lst[0] === "number") {
    return (lst as number[]).reduce(_min_number) as T;
  } else {
    return (lst as bigint[]).reduce(_min_bigint) as T;
  }
};

// gcd
export const gcd = <T extends number | bigint>(a: T, b: T): T => {
  const _gcd_number = (a: number, b: number): number =>
    b === 0 ? a : _gcd_number(b, a % b);
  const _gcd_bigint = (a: bigint, b: bigint): bigint =>
    b === 0n ? a : _gcd_bigint(b, a % b);

  if (typeof a === "number" && typeof b === "number") {
    return _gcd_number(a, b) as T;
  } else if (typeof a === "bigint" && typeof b === "bigint") {
    return _gcd_bigint(a, b) as T;
  } else {
    throw new Error("Abend.");
  }
};

// lcm
export const lcm = <T extends number | bigint>(a: T, b: T): T => {
  const _lcm_number = (a: number, b: number): number => a / gcd(a, b) * b;
  const _lcm_bigint = (a: bigint, b: bigint): bigint => a / gcd(a, b) * b;

  if (typeof a === "number" && typeof b === "number") {
    return _lcm_number(a, b) as T;
  } else if (typeof a === "bigint" && typeof b === "bigint") {
    return _lcm_bigint(a, b) as T;
  } else {
    throw new Error("Abend.");
  }
};

// sum
export const sum = <T extends number | bigint>(lst: T[]): T => {
  const _add_number = (a: number, b: number): number => a + b;
  const _add_bigint = (a: bigint, b: bigint): bigint => a + b;

  if (typeof lst[0] === "number") {
    return (lst as number[]).reduce(_add_number) as T;
  } else {
    return (lst as bigint[]).reduce(_add_bigint) as T;
  }
};

// prod
export const prod = <T extends number | bigint>(lst: T[]): T => {
  const _prod_number = (a: number, b: number): number => a * b;
  const _prod_bigint = (a: bigint, b: bigint): bigint => a * b;

  if (typeof lst[0] === "number") {
    return (lst as number[]).reduce(_prod_number) as T;
  } else {
    return (lst as bigint[]).reduce(_prod_bigint) as T;
  }
};

/*
  polygonal number
 */
export const isTriangle = (n: number): boolean => {
  const tmp = 8 * n + 1;
  const tmp_sqrt = isqrt(tmp);

  return tmp_sqrt * tmp_sqrt === tmp && tmp_sqrt % 2 === 1;
};

export const isSquare = (n: number): boolean => {
  const n_sqrt = isqrt(n);

  return n_sqrt * n_sqrt === n;
};

export const isPentagonal = (n: number): boolean => {
  const tmp = 24 * n + 1;
  const tmp_sqrt = isqrt(tmp);

  return tmp_sqrt * tmp_sqrt === tmp && tmp_sqrt % 6 === 5;
};

export const isHexagonal = (n: number): boolean => {
  const tmp = 8 * n + 1;
  const tmp_sqrt = isqrt(tmp);

  return tmp_sqrt * tmp_sqrt === tmp && tmp_sqrt % 4 === 3;
};

export const getMaxExp = (n: number, base: number): number => {
  let e = 0;
  while (n >= base) {
    n = Math.trunc(n / base);
    e += 1;
  }

  return e;
};

export const checkNtz = (n: number): [number, number] => {
  let cnt = 0;
  while (n % 2 === 0) {
    cnt += 1;
    n = Math.trunc(n / 2);
  }

  return [cnt, n];
};

export const kronecker = (a: number, n: number): number => {
  const mod = (a: number, b: number): number => {
    const tmp = a % b;
    if (tmp >= 0) {
      return tmp;
    } else {
      return (tmp + b);
    }
  };

  if (n === 0) {
    if (Math.abs(a) === 1) {
      return 1;
    } else {
      return 0;
    }
  }

  let sign = 1;
  if (n < 0) {
    if (a < 0) {
      sign = -sign;
    }
    n = -n;
  }

  const [ntz_n, new_n] = checkNtz(n);
  if (ntz_n > 0) {
    if (a % 2 === 0) {
      return 0;
    }
    n = new_n;
    if (ntz_n % 2 === 1) {
      const tmp = a % 8;
      if (tmp === 3 || tmp === 5) {
        sign = -sign;
      }
    }
  }

  while ((a = mod(a, n)) > 0) {
    const [ntz_a, new_a] = checkNtz(a);
    if (ntz_a > 0) {
      a = new_a;
      if (ntz_a % 2 === 1) {
        const tmp = n % 8;
        if (tmp === 3 || tmp === 5) {
          sign = -sign;
        }
      }
    }

    if (a % 4 === 3 && n % 4 === 3) {
      sign = -sign;
    }
    [a, n] = [n, a];
  }

  if (n === 1) {
    return sign;
  } else {
    return 0;
  }
};
