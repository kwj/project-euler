// project euler: problem 22

import { assetData } from "../lib/asset.ts";
import { sum } from "../lib/math.ts";

const calcScore = (words: string[]): number => {
  const score = (word: string): number => {
    return sum(
      word.split("").map((x) => x.charCodeAt(0) - "A".charCodeAt(0) + 1),
    );
  };

  let acc = 0;
  for (const [idx, word] of words.entries()) {
    acc += (idx + 1) * score(word);
  }

  return acc;
};

export const compute = (data: string): string => {
  const keywords = data.replaceAll('"', "").split(",");
  keywords.sort();

  return String(calcScore(keywords));
};

export const solve = (): string => {
  const data = new TextDecoder().decode(assetData("p022_names.txt"));
  return compute(data);
};
