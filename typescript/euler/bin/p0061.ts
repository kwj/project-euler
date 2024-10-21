// project euler: problem 61

import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

const makePolygonalTbl = (
  maxNumSidesPolygon: number,
): Map<number, Map<number, number[]>> => {
  const tbl = new Map<number, Map<number, number[]>>();
  for (const i of range(3, maxNumSidesPolygon + 1)) {
    const mapObj = new Map<number, number[]>();
    const step = i - 2;
    let acc = 0;
    let j = 1;
    while (true) {
      acc += j;
      if (acc >= 10_000) {
        break;
      }
      if (acc >= 1_000 && acc % 100 >= 10) {
        const key = Math.trunc(acc / 100);
        const val = acc % 100;
        mapObj.set(key, (mapObj.get(key) || []).concat([val]));
      }
      j += step;
    }
    tbl.set(i, mapObj);
  }

  return tbl;
};

const findClosedPaths = (maxNumSidesPolygon: number): number[][] => {
  const paths: number[][] = [];
  const polyTbl = makePolygonalTbl(maxNumSidesPolygon);

  // example: (when maxNumSidesPolygon = 8)
  //   0b######000
  //     ||||||
  //     |||||+- triangle
  //     ||||+-- square
  //     |||+--- pentagonal
  //     ||+---- hexagonal
  //     |+----- heptagonal
  //     +------ octagonal
  const stopCondition = (1 << (maxNumSidesPolygon + 1)) - 8;

  const getNextState = (state: [number, number[]]): [number, number[]][] => {
    const states: [number, number[]][] = [];
    const bits = state[0];
    const path = state[1];
    if (bits == stopCondition && path.at(0)! == path.at(-1)!) {
      paths.push(path);
    } else {
      for (const i of range(3, maxNumSidesPolygon)) {
        const p_bit = 1 << i;
        if ((bits & p_bit) != 0) {
          continue;
        }
        const next_tbl = polyTbl.get(i)!;
        if (next_tbl.has(path.at(-1)!)) {
          for (const x of next_tbl.get(path.at(-1)!)!) {
            states.push([bits | p_bit, [...path, x]]);
          }
        }
      }
    }

    return states;
  };

  // search by DFS
  const q: [number, number[]][] = [];
  for (const [k, vs] of polyTbl.get(maxNumSidesPolygon)!.entries()) {
    for (const v of vs) {
      q.push([1 << maxNumSidesPolygon, [k, v]]);
    }
  }
  while (q.length > 0) {
    //console.log(q);
    const state = q.pop()!;
    for (const next_state of getNextState(state)) {
      q.push(next_state);
    }
  }

  return paths;
};

export const compute = (maxNumSidesPolygon: number): string => {
  const isDistinctNumbers = (lst: number[]): boolean => {
    const tmp: Set<number> = new Set();
    for (const i of range(0, lst.length - 1)) {
      tmp.add(lst[i] * 100 + lst[i + 1]);
    }
    return tmp.size == lst.length - 1;
  };

  const cycles: number[][] = [];
  for (const path of findClosedPaths(maxNumSidesPolygon)) {
    // All numbers in a cycle are different from each other's
    if (isDistinctNumbers(path)) {
      cycles.push(path);
    }
  }

  // There exists only one cycle
  if (cycles.length == 1) {
    return String(sum(cycles[0].slice(1)) * 101);
  }

  throw new Error("unreachable");
};

export const solve = (): string => compute(8);
