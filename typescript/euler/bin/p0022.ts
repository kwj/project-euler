
// project euler: problem 22

import { assetData } from "../lib/asset.ts";
import { sum } from "../lib/math.ts";

function calcScore(words: string[]): number {
  function score(word: string): number {
    return sum(word.split("").map((x) => x.charCodeAt(0) - "A".charCodeAt(0) + 1));
  }

  let acc = 0;
  for (const [idx, word] of words.entries()) {
    acc += (idx + 1) * score(word);
  }

  return acc;
}

export function compute(data: string): string {
  const keywords = data.replaceAll('"', "").split(",");
  keywords.sort();

  return String(calcScore(keywords));
}

export function solve(): void {
  try {
    const data = new TextDecoder().decode(assetData("p022_names.txt"));

    const t0 = performance.now();
    const result = compute(data);
    const t1 = performance.now();
    const duration_ms = (t1 - t0).toFixed(4);

    console.log(`Answer: ${result}`);
    console.log(`Elapsed time: ${duration_ms} msec.`);
  } catch (err) {
    console.error(err.message);
  }

  return;
}
