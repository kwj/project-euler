// project euler: problem 98

import { zip } from "@std/collections";
import { assetData } from "../lib/asset.ts";
import { isqrt, maxLst } from "../lib/math.ts";
import { dedupSort, range } from "../lib/util.ts";

class SquareTbl {
  tbl: Map<number, Set<string>>;

  constructor() {
    this.tbl = new Map<number, Set<string>>();
  }

  get(k: number): Set<string> {
    if (this.tbl.has(k)) {
      return this.tbl.get(k)!;
    } else {
      const sqSet = new Set(
        range(isqrt(10 ** (k - 1)), isqrt(10 ** k - 1) + 1)
          .map((x) => x * x)
          .filter((x) => x >= 10 ** (k - 1))
          .map((x) => String(x)),
      );
      this.tbl.set(k, sqSet);

      return sqSet;
    }
  }
}

const findSquares = (w1: string, w2: string, sqSet: Set<string>): number[] => {
  const result: Set<number> = new Set();

  for (const sq of sqSet.values()) {
    for (const n of makeNumber(w1, w2, sq)) {
      if (sqSet.has(n)) {
        result.add(Number(n));
        result.add(Number(sq));
      }
    }
  }

  return Array.from(result.values());
};

const makeNumber = (w1: string, w2: string, sq: string): string[] => {
  const lookup = (ch: string, map: [string, string][]): string => {
    for (const [k, v] of map) {
      if (k === ch) {
        return v;
      }
    }

    return "";
  };

  const transPair = dedupSort(zip(w1.split(""), sq.split("")));
  if (transPair.length !== dedupSort(w1.split("")).length) {
    return [];
  }
  if (transPair.length !== dedupSort(transPair.map(([_, x]) => x)).length) {
    return [];
  }

  return [w2.split("").map((x) => lookup(x, transPair)).join("")];
};

export const compute = (data: string): string => {
  const parseData = (data: string): [number, string, string][] => {
    const keywords = data.replaceAll('"', "").split(",");

    return keywords.map((w) => [w.length, w.split("").sort().join(""), w]);
  };

  const isAnagram = (
    a: [number, string, string],
    b: [number, string, string],
  ): boolean => {
    return a[0] === b[0] && a[1] === b[1];
  };

  let numLst: number[] = [];
  const sqTbl = new SquareTbl();
  const words = parseData(data);
  for (const i of range(0, words.length - 1)) {
    for (const j of range(i + 1, words.length)) {
      if (!isAnagram(words[i], words[j])) {
        continue;
      }
      numLst = numLst.concat(
        findSquares(words[i][2], words[j][2], sqTbl.get(words[i][0])!),
      );
    }
  }

  if (numLst.length === 0) {
    throw new Error("there is no answer");
  } else {
    return String(maxLst(numLst));
  }
};

export const solve = (): string => {
  const data = new TextDecoder().decode(assetData("p098_words.txt"));
  return compute(data);
};
