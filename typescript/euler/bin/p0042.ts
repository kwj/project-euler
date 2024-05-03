// project euler: problem 42

import { zip } from "std/collections/zip.ts";
import { assetData } from "../lib/asset.ts";
import { isTriangle, sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

const calcScore = (words: string[]): number[] => {
  const score = (word: string): number =>
    sum(word.split("").map((x) => tbl.get(x) as number));

  const AtoZ = range("A".charCodeAt(0), "Z".charCodeAt(0) + 1)
    .map((x) => String.fromCharCode(x));
  const tbl = new Map<string, number>(zip(AtoZ, range(1, AtoZ.length + 1)));

  return words.map((x) => score(x));
};

export const compute = (data: string): string => {
  const keywords = data.replaceAll('"', "").split(",");

  return String(
    calcScore(keywords).filter((x) => isTriangle(x) === true).length,
  );
};

export const solve = (): string => {
  const data = new TextDecoder().decode(assetData("p042_words.txt"));
  return compute(data);
};
