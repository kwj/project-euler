/*
  Return the number of binary digits.

  > Number(10).toString(2)
  "1010"
  > bitLength(10)
  4

  > bitLength(2n ** 128n - 1n)
  128
*/

const bitLengthNumber = (n: number): number => {
  function aux(n: number, cnt: number): number {
    if (n === 0) {
      return cnt;
    } else {
      return aux(Math.trunc(n / 2), cnt + 1);
    }
  }

  if (n === 0) {
    return 0;
  } else {
    return aux(n, 0);
  }
};

const bitLengthBigint = (n: bigint): number => {
  function aux(n: bigint, cnt: number): number {
    if (n === 0n) {
      return cnt;
    } else {
      return aux(n / 2n, cnt + 1);
    }
  }

  if (n === 0n) {
    return 0;
  } else {
    return aux(n, 0);
  }
};

export const bitLength = (n: number | bigint): number => {
  if (typeof n === "number") {
    return bitLengthNumber(n);
  } else {
    return bitLengthBigint(n);
  }
};

/*
  start: included
  stop: not included
*/
export const range = (start: number, stop: number, step = 1): number[] => {
  if (start > stop && step > 0) {
    return [];
  } else if (start < stop && step < 0) {
    return [];
  } else if (step === 0 || start === stop) {
    return [];
  }

  const flag = (start < stop) ? -1 : 1;
  const length = Math.trunc(((stop + flag) - start) / step) + 1;
  return Array.from({ length }, (_, i) => start + (i * step));
};

export const numOfDigits = (n: number): number => {
  const [_, e] = n.toExponential().split("e");
  if (e[0] === "+") {
    // abs(n) >= 1 or n = 0
    return Number(e) + 1;
  } else {
    return 1;
  }
};

export const numOfDecimals = (n: number): number => {
  const [_, d] = String(n).split(".");
  return d.length;
};

export const isPandigital = (n: number): boolean => {
  function mkBits(n: number) {
    let bits = 0;
    while (n > 0) {
      bits |= 1 << (n % 10);
      n = Math.trunc(n / 10);
    }

    return bits;
  }

  return mkBits(n) === ((1 << numOfDigits(n)) - 1);
};

export const isPandigitalNZ = (n: number): boolean => {
  function checkZero(n: number): boolean {
    while (n > 0) {
      if (n % 10 === 0) {
        return false;
      }
      n = Math.trunc(n / 10);
    }

    return true;
  }

  return checkZero(n) && isPandigital(n * 10);
};

export const cmpLst = <T>(lst1: T[], lst2: T[]): number => {
  const len = Math.min(lst1.length, lst2.length);
  for (const i of range(0, len)) {
    if (lst1[i] > lst2[i]) {
      return 1;
    } else if (lst1[i] < lst2[i]) {
      return -1;
    }
  }

  if (lst1.length > lst2.length) {
    return 1;
  } else if (lst1.length < lst2.length) {
    return -1;
  } else {
    return 0;
  }
};

interface Counter {
  [prop: string]: number;
}

export const Counter = <T>(lst: T[]): [string, number][] => {
  const cntr: Counter = {};
  lst.forEach((x) => cntr[String(x)] = (cntr[String(x)] || 0) + 1);

  return Object.entries(cntr);
};

export const isPalindrome = (num: number, base = 10): boolean => {
  let x = num;
  let acc = 0;
  while (x > 0) {
    acc = acc * base + (x % base);
    x = Math.trunc(x / base);
  }

  return num === acc;
};

export const dedupSort = <T>(
  lst: T[],
  compareFn?: (a: T, b: T) => number,
): T[] => {
  const elms = new Set<T>();
  const result: T[] = [];

  for (const x of lst) {
    if (!elms.has(x)) {
      elms.add(x);
      result.push(x);
    }
  }

  if (compareFn === undefined) {
    return result.sort();
  } else {
    return result.sort(compareFn);
  }
};

export const assocGroupMap = <T, U>(tplLst: [T, U][]): Map<T, U[]> => {
  const result = new Map<T, U[]>();
  for (const [k, v] of tplLst) {
    const new_v = !result.has(k) ? [v] : result.get(k)!.concat(v);
    result.set(k, new_v);
  }

  return result;
};
