// project euler: problem 59

import { permutationsWithReplacement } from "combinatorics/mod.ts";
import { assetData } from "../lib/asset.ts";
import { sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

const decode = (cipherText: [number, number][], key: number[]): number[] =>
  cipherText.map(([e, i]) => e ^ key[i]);

const calcScore = (lst: number[]): number => {
  const wordLst = lst.map((x) => String.fromCharCode(x)).join("").split(/[,. ]/)
    .map((x) => x.toLocaleLowerCase());

  let cnt = 0;
  for (const word of ["and", "or", "a", "to", "in"]) {
    cnt += wordLst.filter((x) => x === word).length;
  }

  return cnt;
};

export const compute = (data: string): string => {
  const cipherText: [number, number][] = data.split(",").map((
    x,
    idx,
  ) => [Number(x), idx % 3]);
  let maxScore: [number, number[], number[]] = [0, [], []];

  for (
    const key of permutationsWithReplacement(
      range("a".charCodeAt(0), "z".charCodeAt(0) + 1),
      3,
    )
  ) {
    const plainText = decode(cipherText, key);
    const score = calcScore(plainText);
    if (score > maxScore[0]) {
      maxScore = [score, key, plainText];
    }
  }
  return String(sum(maxScore[2]));
};

export const solve = (): string => {
  const data = new TextDecoder().decode(assetData("p059_cipher.txt"));
  return compute(data);
};
