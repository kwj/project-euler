// project euler: problem 62

export const compute = (numOfPerms: number): string => {
  function makeKey(n: number): string {
    return String(n).split("").sort().join("");
  }

  const tbl = new Map<string, number[]>();
  let n = 0;
  while (true) {
    n += 1;
    const key = makeKey(n * n * n);
    if (tbl.has(key) === true) {
      tbl.set(key, tbl.get(key)!.concat(n));
      if (tbl.get(key)!.length === numOfPerms) {
        return String(tbl.get(key)![0] ** 3);
      }
    } else {
      tbl.set(key, [n]);
    }
  }
};

export const solve = (): string => compute(5);
