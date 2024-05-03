// project euler: problem 82

import { assetData } from "../lib/asset.ts";
import { range } from "../lib/util.ts";

export const compute = (
  fn: (...valus: number[]) => number,
  data: string,
): string => {
  function parseData(data: string): number[][] {
    function splitLines(str: string): string[] {
      const result = str.split(/\r?\n/);
      if (result.at(-1) === "") {
        return result.slice(0, -1);
      } else {
        return result;
      }
    }

    return splitLines(data).map((x) => x.split(",").map((y) => Number(y)));
  }

  function transpose(matrix: number[][]): number[][] {
    // Add a check process for square matrix if necessary
    return matrix[0].map((_, idx) => matrix.map((row) => row[idx]));
  }

  const matrix = transpose(parseData(data));
  const work = matrix[0];
  for (const crnt of matrix.slice(1)) {
    work[0] += crnt[0];
    for (const i of range(1, crnt.length)) {
      work[i] = crnt[i] + fn(work[i], work[i - 1]);
    }
    for (const i of range(0, crnt.length - 1).reverse()) {
      work[i] = fn(work[i], work[i + 1] + crnt[i]);
    }
  }

  return String(work.sort()[0]);
};

export const solve = (): string => {
  const data = new TextDecoder().decode(assetData("p082_matrix.txt"));
  return compute(Math.min, data);
};
