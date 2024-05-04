// project euler: problem 67

import { zip } from "@std/collections";
import { assetData } from "../lib/asset.ts";

const selectLeaf = (
  fn: (...valus: number[]) => number,
  lst: number[],
): number[] => {
  const result = [];
  let prev = lst[0];
  for (const i of lst) {
    result.push(fn(prev, i));
    prev = i;
  }

  return result.slice(1);
};

export const compute = (
  fn: (...valus: number[]) => number,
  data: string,
): string => {
  const parseData = (data: string): number[][] => {
    const splitLines = (str: string): string[] => {
      const result = str.split(/\r?\n/);
      if (result.at(-1) === "") {
        return result.slice(0, -1);
      } else {
        return result;
      }
    };

    return splitLines(data).map((x) => x.split(" ").map((y) => Number(y)));
  };

  const nums = parseData(data);
  nums.reverse();
  let prev = nums[0];
  for (const lst of nums.slice(1)) {
    prev = zip(lst, selectLeaf(fn, prev)).map((tpl) => tpl[0] + tpl[1]);
  }

  return String(prev[0]);
};

export const solve = (): string => {
  const data = new TextDecoder().decode(assetData("p067_triangle.txt"));
  return compute(Math.max, data);
};
