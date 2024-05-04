// project euler: problem 99

import { zip } from "@std/collections";
import { assetData } from "../lib/asset.ts";
import { range } from "../lib/util.ts";

export const compute = (data: string): string => {
  const parseData = (data: string): [number, number][] => {
    const splitLines = (str: string): string[] => {
      const result = str.split(/\r?\n/);
      if (result.at(-1) === "") {
        return result.slice(0, -1);
      } else {
        return result;
      }
    };

    return splitLines(data).map((x) => x.split(",")).map((
      x,
    ) => [Number(x[0]), Number(x[1])]);
  };

  const calc_result = parseData(data).map((tpl) => tpl[1] * Math.log10(tpl[0]));

  return String(
    zip(range(1, calc_result.length + 1), calc_result).sort((a, b) =>
      Number(b[1]) - Number(a[1])
    )[0][0],
  );
};

export const solve = (): string => {
  const data = new TextDecoder().decode(assetData("p099_base_exp.txt"));
  return compute(data);
};
