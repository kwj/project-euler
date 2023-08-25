/*
 *  Prime functions
 */

import { unzip } from "std/collections/unzip.ts";
import { zip } from "std/collections/zip.ts";
import { isqrt, modPow } from "./math.ts";
import { randInt, range } from "./util.ts";

/* Primality test (trial division) */

export function isPrimeSimple(n: number): boolean {
  if (n === 2) {
    return true;
  } else if (n < 2) {
    return false;
  } else if (n % 2 === 0) {
    return false;
  } else {
    for (const x of range(3, isqrt(n) + 1, 2)) {
      if (n % x === 0) {
        return false;
      }
    }
    return true;
  }
}

/* Primality test (Miller-Rabin) */

enum NumType {
  Prime,
  Composite,
  Undecided,
}

// Deterministic variants of the Miller-Rabin primality test (n <= 2 ** 64)
//   http://miller-rabin.appspot.com/
export function isPrime(num: number): boolean {
  function distinguish(a: bigint, d: bigint, s: bigint, n: bigint): NumType {
    let x = modPow(a, d, n);
    if (x === 0n) {
      return NumType.Prime;
    } else if (x === 1n || x === n - 1n) {
      return NumType.Undecided;
    } else {
      for (let i = 0n; i < s; i = i + 1n) {
        x = modPow(x, 2n, n);
        if (x === 0n) {
          return NumType.Prime;
        } else if (x === n - 1n) {
          return NumType.Undecided;
        }
      }
      return NumType.Composite;
    }
  }

  if (num < 2) {
    return false;
  }

  let d = BigInt(num - 1);
  let s = 0n;
  while (d % 2n === 0n) {
    d = d / 2n;
    s = s + 1n;
  }
  const n = BigInt(num);

  for (const a of [2n, 325n, 9375n, 28178n, 450775n, 9780504n, 1795265022n]) {
    const result = distinguish(a, d, s, n);
    if (result === NumType.Prime) {
      return true;
    } else if (result === NumType.Composite) {
      return false;
    }
  }
  return true;
}

// Normal Miller-Rabin primality test
export function isProbablyPrime(num: bigint): boolean {
  function distinguish(a: bigint, d: bigint, s: bigint, n: bigint): NumType {
    let x = modPow(a, d, n);
    if (x === 1n || x === n - 1n) {
      return NumType.Undecided;
    } else {
      for (let i = 0n; i < s; i = i + 1n) {
        x = modPow(x, 2n, n);
        if (x === n - 1n) {
          return NumType.Undecided;
        }
      }
      return NumType.Composite;
    }
  }

  function rand_arr(len: number): number[] {
    const arr: number[] = [];
    // 43 is next larger prime number than 41
    for (let i = 0; i < len; i++) {
      arr.push(randInt(43, Number.MAX_SAFE_INTEGER));
    }
    return arr;
  }

  if (num < 2n) {
    return false;
  }

  // Number.MAX_SAFE_INTEGER = 9007199254740991
  if (num <= 9007199254740991n) {
    return isPrime(Number(num));
  }

  let d = num - 1n;
  let s = 0n;
  while (d % 2n === 0n) {
    d = d / 2n;
    s = s + 1n;
  }
  for (const a of [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41].concat(rand_arr(20))) {
    const result = distinguish(BigInt(a), d, s, num);
    if (result === NumType.Composite) {
      return false;
    }
  }
  // probably prime
  return true;
}

/* Prime number generator */

export function* primeGenerator(): Generator<number, void, unknown> {
  let p = 2;
  const tbl = new Map();
  while (true) {
    if (tbl.has(p) == true) {
      for (const n of tbl.get(p)) {
        if (tbl.has(p + n) === true) {
          tbl.set(p + n, tbl.get(p + n).concat([n]));
        } else {
          tbl.set(p + n, [n]);
        }
      }
      tbl.delete(p);
    } else {
      tbl.set(p ** 2, [p]);
      yield p;
    }
    p += 1;
  }
}

/* Sieve of Eratosthenes (Simple version) */

export function getPrimes(limit: number): number[] {
  const primes = Array.from(range(0, limit), (x) => x % 2 === 0 ? 0 : x);
  primes[2] = 2;

  for (const i of range(3, limit + 1, 2)) {
    if (primes[i] != 0) {
      for (const j of range(i * i, limit + 1, i)) {
        primes[j] = 0;
      }
    }
  }

  return primes.filter((x) => x > 1);
}

export function getPrimeTbl(limit: number): boolean[] {
  const prime_tbl: boolean[] = new Array(limit + 1).fill(true).map((x, idx) => idx % 2 === 0 ? false : x);
  prime_tbl[1] = false;
  prime_tbl[2] = true;

  for (const i of range(3, limit + 1, 2)) {
    if (prime_tbl[i] === true) {
      for (const j of range(i * i, limit + 1, i)) {
        prime_tbl[j] = false;
      }
    }
  }

  return prime_tbl;
}

export function primeTblToPrimes(lst: boolean[]): number[] {
  const [_, primes] = unzip(zip(lst, range(0, lst.length)).filter((tpl) => tpl[0] === true));

  return primes;
}

/* Sieve of Eratosthenes (Segmented sieve version) */

export class Sieve {
  #begin = 0;
  #end = 0;
  #mini_primes: number[] = [];
  #mini_tbl: number[] = [];
  #primes: number[] = [];
  #prime_tbl: number[] = [];

  constructor(start?: number, stop?: number) {
    if (start != undefined && stop === undefined) {
      if (start <= 1) {
        throw new RangeError("The value must be larger than 1.");
      }
      this.#begin = 1;
      this.#end = start;
    } else if (start != undefined && stop != undefined) {
      if (start < 1) {
        throw new RangeError("The start value must be larger than 0.");
      } else if (start > stop) {
        throw new RangeError("The start value must be smaller than the end value.");
      } else {
        this.#begin = start;
        this.#end = stop;
      }
    }

    if (this.#end != 0) {
      this.#updateMiniTbl();
      this.#makePrimeTbl();
    }
  }

  #updateMiniTbl(newSize = 0): void {
    const startPos = (n: number): number => {
      const offset = (ext_begin % n) != 0 ? n - (ext_begin % n) : 0;

      return Math.max(n * n, ext_begin + offset);
    };

    const limit = isqrt(Math.max(this.#end, newSize));
    const ext_begin = (newSize != 0) ? this.#mini_tbl.length : 0;
    this.#mini_tbl = this.#mini_tbl.concat(range(ext_begin, limit + 1));

    for (const n of range(startPos(2), limit + 1, 2)) {
      this.#mini_tbl[n] = 0;
    }
    for (const i of range(3, limit + 1, 2)) {
      if (this.#mini_tbl[i] != 0) {
        for (const j of range(startPos(i), limit + 1, i)) {
          this.#mini_tbl[j] = 0;
        }
      }
    }

    this.#mini_primes = this.#mini_primes.concat(this.#mini_tbl.slice(ext_begin).filter((x) => x > 1));

    return;
  }

  #makePrimeTbl(): void {
    const startPos = (n: number): number => {
      const offset = this.#begin % n != 0 ? n - (this.#begin % n) : 0;

      return Math.max(n * n, this.#begin + offset) - this.#begin;
    };

    this.#prime_tbl = range(this.#begin, this.#end + 1);
    for (const i of this.#mini_primes) {
      if (i * i > this.#end) {
        break;
      }
      for (const j of range(startPos(i), this.#end + 1, i)) {
        this.#prime_tbl[j] = 0;
      }
    }

    this.#primes = this.#prime_tbl.filter((x) => x > 1);

    return;
  }

  getPrimes(): number[] {
    if (this.#end != 0) {
      return this.#primes;
    } else {
      throw new Error("not initialized");
    }
  }

  isPrime(n: number): boolean {
    if (n > this.#end) {
      throw new RangeError("too large");
    } else if (this.#end != 0) {
      return this.#prime_tbl[n - this.#begin] > 1;
    } else {
      throw new Error("not initialized");
    }
  }

  update(start: number, stop: number): void {
    if (this.#end != 0) {
      if (start > stop) {
        throw new RangeError(`The start value must be smaller than the end value. (start=${start}, stop=${stop})`);
      } else if (start < 1) {
        throw new RangeError("The start value must be larger than 0.");
      } else {
        if (stop > this.#end) {
          this.#updateMiniTbl(stop);
        }
        this.#begin = start;
        this.#end = stop;
        this.#makePrimeTbl();
      }
    } else {
      if (start < 1) {
        throw new RangeError("The start value must be larger than 0.");
      } else if (start > stop) {
        throw new RangeError(`The start value must be smaller than the end value. (start=${start}, stop=${stop})`);
      } else {
        this.#begin = start;
        this.#end = stop;
        this.#updateMiniTbl();
        this.#makePrimeTbl();
      }
    }

    return;
  }
}
