// project euler: problem 98

import { unzip, zip } from "@std/collections";
import { combinations } from "combinatorics/mod.ts";
import { assetData } from "../lib/asset.ts";
import { isqrt, max } from "../lib/math.ts";
import { assocGroupMap, dedupSort, numOfDigits } from "../lib/util.ts";

const selectKeywords = (words: string[]) => {
  const makeKey = (word: string): string => word.split("").sort().join("");

  const makeKWinfo = (
    tplLst: [string, string][],
  ): [number, [string, string[]][]] => {
    const work = assocGroupMap(tplLst);
    const [keys, _] = unzip(
      [...work.entries()].filter(([_, vLst]) => vLst.length === 1),
    );
    for (const key of keys) {
      work.delete(key);
    }

    const result = [...work.entries()].sort((a, b) =>
      b[0].length - a[0].length
    );

    return [result[0][0].length, result];
  };

  return makeKWinfo(words.map((x) => [makeKey(x), x]));
};

const makeSquareTbl = (
  digits: number,
): [Map<number, string[]>, Map<number, string[]>] => {
  const limit = isqrt(10 ** digits - 1);
  const work: [number, string][] = [];

  let n = 1;
  while (n <= limit) {
    const sq = n * n;
    work.push([numOfDigits(sq), String(sq)]);
    n += 1;
  }

  const sq_tbl = assocGroupMap(work);
  const sq_uniq_tbl = new Map<number, string[]>();
  for (const [k, vLst] of sq_tbl.entries()) {
    const uniq_vLst = vLst.filter((x) =>
      x.length === dedupSort(x.split("")).length
    );
    if (uniq_vLst.length !== 0) {
      sq_uniq_tbl.set(k, uniq_vLst);
    }
  }

  return [sq_tbl, sq_uniq_tbl];
};

const translate = (s: string, transMap: Map<string, string[]>): string => {
  const result: string[] = [];
  for (const ch of s.split("")) {
    result.push(transMap.get(ch)![0]);
  }

  return result.join("");
};

export const compute = (data: string): string => {
  const checkPair = (w1: string, w2: string): number | undefined => {
    const ndigits = w1.length;
    const tbl = (ndigits === dedupSort(w1.split("")).length)
      ? sq_uniq_tbl
      : sq_tbl;

    for (const sq of tbl.get(ndigits)!) {
      const transMap = assocGroupMap(zip(w1.split(""), sq.split("")));
      if (transMap.size === ndigits) {
        const w2_trans = translate(w2, transMap);
        if (tbl.get(ndigits)!.includes(w2_trans) === true) {
          return max(Number(translate(w1, transMap)), Number(w2_trans));
        }
      }
    }

    return undefined;
  };

  const words = data.replaceAll('"', "").split(",");
  const [maxDigits, kw] = selectKeywords(words);
  const [sq_tbl, sq_uniq_tbl] = makeSquareTbl(maxDigits);

  const [_, wLst] = unzip(kw);
  for (const v of wLst) {
    for (const [w1, w2] of combinations(v, 2)) {
      const result = checkPair(w1, w2);
      if (result !== undefined) {
        return String(result);
      }
    }
  }

  throw new Error("not reached");
};

export const solve = (): string => {
  const data = new TextDecoder().decode(assetData("p098_words.txt"));
  return compute(data);
};
