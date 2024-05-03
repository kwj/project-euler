// project euler: problem 90

import {
  cartesianProduct,
  combinations,
  permutationsWithReplacement,
} from "combinatorics/mod.ts";
import { dedupSort } from "../lib/util.ts";

function proc69(lst: string[]): string[] {
  if (lst.includes("6") === true || lst.includes("9") === true) {
    return dedupSort(lst.concat("6", "9"));
  } else {
    return lst;
  }
}

function makeNumbers(d1: string[], d2: string[]): string[] {
  const result: string[] = [];
  for (const tpl of cartesianProduct(d1, d2)) {
    result.push(tpl[0] + tpl[1], tpl[1] + tpl[0]);
  }

  return result;
}

export const compute = (): string => {
  const squares = ["01", "04", "09", "16", "25", "36", "49", "64", "81"];
  const faces: string[][] = [];
  for (
    const lst of combinations(
      ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"],
      6,
    )
  ) {
    faces.push(proc69(lst));
  }

  let acc = 0;
  for (const [d1, d2] of permutationsWithReplacement(faces, 2)) {
    const numbers = makeNumbers(d1, d2);
    if (squares.every((x) => numbers.includes(x)) === true) {
      acc += 1;
    }
  }

  return String(Math.trunc(acc / 2));
};

export const solve = (): string => compute();
