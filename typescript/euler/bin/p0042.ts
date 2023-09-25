// project euler: problem 42

import { zip } from "std/collections/zip.ts";
import { assetData } from "../lib/asset.ts";
import { isTriangle, sum } from "../lib/math.ts";
import { range } from "../lib/util.ts";

function calcScore(words: string[]): number[] {
  function score(word: string): number {
    return sum(word.split("").map((x) => tbl.get(x) as number));
  }

  const AtoZ = range("A".charCodeAt(0), "Z".charCodeAt(0) + 1).map((x) =>
    String.fromCharCode(x)
  );
  const tbl = new Map<string, number>(zip(AtoZ, range(1, AtoZ.length + 1)));

  return words.map((x) => score(x));
}

export function compute(data: string): string {
  const keywords = data.replaceAll('"', "").split(",");

  return String(
    calcScore(keywords).filter((x) => isTriangle(x) === true).length,
  );
}

export function solve(): void {
  try {
    const data = new TextDecoder().decode(assetData("p042_words.txt"));

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
