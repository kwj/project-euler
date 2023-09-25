// project euler: problem 99

import { zip } from "std/collections/zip.ts";
import { assetData } from "../lib/asset.ts";
import { range } from "../lib/util.ts";

export function compute(data: string): string {
  function parseData(data: string): [number, number][] {
    function splitLines(str: string): string[] {
      const result = str.split(/\r?\n/);
      if (result.at(-1) === "") {
        return result.slice(0, -1);
      } else {
        return result;
      }
    }

    return splitLines(data).map((x) => x.split(",")).map((
      x,
    ) => [Number(x[0]), Number(x[1])]);
  }

  const calc_result = parseData(data).map((tpl) => tpl[1] * Math.log10(tpl[0]));

  return String(
    zip(range(1, calc_result.length + 1), calc_result).sort((a, b) =>
      Number(b[1]) - Number(a[1])
    )[0][0],
  );
}

export function solve(): void {
  try {
    const data = new TextDecoder().decode(assetData("p099_base_exp.txt"));

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
