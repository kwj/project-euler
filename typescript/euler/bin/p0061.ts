// project euler: problem 61

import { permutations } from "combinatorics/mod.ts";
import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

const makePolygonalTbl = () => {
  const trunc = Math.trunc;
  const fn = new Map<number, (n: number) => number>([
    [3, (n) => trunc(n * (n + 1) / 2)],
    [4, (n) => n * n],
    [5, (n) => trunc(n * (3 * n - 1) / 2)],
    [6, (n) => n * (2 * n - 1)],
    [7, (n) => trunc(n * (5 * n - 3) / 2)],
    [8, (n) => n * (3 * n - 2)],
  ]);

  const tbl = new Map<number, Map<number, number[]>>();
  for (const i of range(3, 8 + 1)) {
    const mapObj = new Map<number, number[]>();
    let j = 0;

    while (true) {
      j += 1;
      const num = fn.get(i)!(j);
      if (num < 1_000) {
        continue;
      } else if (num >= 10_000) {
        break;
      } else if (num % 100 < 10) {
        continue;
      }

      const key = trunc(num / 100);
      const val = num % 100;
      mapObj.set(key, (mapObj.get(key) || []).concat([val]));
    }
    tbl.set(i, mapObj);
  }

  return tbl;
};

const findCycle = (
  polyTbl: Map<number, Map<number, number[]>>,
  route: number[],
): number[] | undefined => {
  const dfs = (nextRoute: number[], path: number[]): number[] | undefined => {
    if (nextRoute.length === 0) {
      return (path[0] === path.at(-1)) ? path.slice(1) : undefined;
    }

    const nextHop = nextRoute[0];
    const nextMap = polyTbl.get(nextHop)!;
    if (!nextMap.has(path[0])) {
      return undefined;
    }
    for (const nextNum of nextMap.get(path[0])!) {
      const result = dfs(nextRoute.slice(1), [nextNum].concat(path));
      if (result !== undefined) {
        return result;
      }
    }

    return undefined;
  };

  // Start searching from octagonal numbers
  for (const [k, v] of polyTbl.get(8)!.entries()) {
    for (const nextNum of v) {
      const result = dfs(route, [nextNum, k]);
      if (result !== undefined) {
        return result;
      }
    }
  }

  return undefined;
};

// Assume that octagonal numbers are the start/goal positions on cycle
export const compute = (): string => {
  const polyTbl = makePolygonalTbl();

  // There is only one cycle path exist, so it terminates immediately if a cycle found
  for (const route of permutations(range(3, 7 + 1))) {
    const result = findCycle(polyTbl, route);
    if (result !== undefined) {
      // sum(100*x{1} + x{2}, 100*x{2} + x{3}, ..., 100*x{n} + x{1}) = sum(x{1}, x{2}, ..., x{n}) * 101
      return String(sum(result) * 101);
    }
  }

  throw new Error("not found");
};

export const solve = (): string => compute();
