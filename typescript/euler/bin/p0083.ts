// project euler: problem 83

import { ascend, BinaryHeap } from "std/data_structures/mod.ts";
import { assetData } from "../lib/asset.ts";
import { range } from "../lib/util.ts";

const makeNeighborTbl = (
  rows: number,
  cols: number,
): [number, number][][][] => {
  let tbl: [number, number][][][] = [];
  for (const _ of range(0, rows)) {
    tbl = tbl.concat([new Array(cols).fill([])]);
  }

  for (const r of range(0, rows)) {
    for (const c of range(0, cols)) {
      //console.log(  [[r - 1, c], [r + 1, c], [r, c - 1], [r, c + 1]].filter((x) => x[0] >= 0 && x[0] < rows && x[1] >= 0 && x[1] < cols) as [number, number][] );
      tbl[r][c] = [[r - 1, c], [r + 1, c], [r, c - 1], [r, c + 1]].filter((x) =>
        x[0] >= 0 && x[0] < rows && x[1] >= 0 && x[1] < cols
      ) as [number, number][];
    }
  }
  return tbl;
};

const makeDistanceTbl = (rows: number, cols: number): number[][] => {
  let tbl: number[][] = [];
  for (const _ of range(0, rows)) {
    tbl = tbl.concat([new Array(cols).fill(Number.MAX_SAFE_INTEGER)]);
  }

  return tbl;
};

export const compute = (data: string): string => {
  const parseData = (data: string): number[][] => {
    const splitLines = (str: string): string[] => {
      const result = str.split(/\r?\n/);
      if (result.at(-1) === "") {
        return result.slice(0, -1);
      } else {
        return result;
      }
    };

    return splitLines(data).map((x) => x.split(",").map((y) => Number(y)));
  };

  const matrix = parseData(data);
  const nbrTbl = makeNeighborTbl(matrix.length, matrix[0].length);
  const distTbl = makeDistanceTbl(matrix.length, matrix[0].length);
  distTbl[0][0] = matrix[0][0];

  const pq = new BinaryHeap<[number, [number, number]]>((a, b) =>
    ascend(a[0], b[0])
  );
  pq.push([distTbl[0][0], [0, 0]]);

  while (pq.isEmpty() === false) {
    const [d, [i, j]] = pq.pop() as [number, [number, number]];
    for (const [x, y] of nbrTbl[i][j]) {
      const new_d = d + matrix[x][y];
      if (new_d < distTbl[x][y]) {
        distTbl[x][y] = new_d;
        pq.push([new_d, [x, y]]);
      }
    }
  }

  return String(distTbl.at(-1)!.at(-1));
};

export const solve = (): string => {
  const data = new TextDecoder().decode(assetData("p083_matrix.txt"));
  return compute(data);
};
