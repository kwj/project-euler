
// project euler: problem 89

/*
  step 1:
    IIIIIIIII     IX
    XXXXXXXXX     XC
    CCCCCCCCC     CM

  step 2:
    VIIII         IX
    LXXXX         XC
    DCCCC         CM

  step 3:
    IIIII         V
    XXXXX         L
    CCCCC         D

  step 4:
    IIII          IV
    XXXX          XL
    CCCC          CD

  We will need the following files to run this program.
    - https://projecteuler.net/project/resources/p089_roman.txt
*/

import { assetData } from "../lib/asset.ts";

function replaceNumbers(line: string): string {
  // step 1, 2, 3 and 4
  let s = line.replace(/IIIIIIIII|XXXXXXXXX|CCCCCCCCC/g, "##");
  s = s.replace(/VIIII|LXXXX|DCCCC/g, "##");
  s = s.replace(/IIIII|XXXXX|CCCCC/g, "#");
  s = s.replace(/IIII|XXXX|CCCC/g, "##");

  return s;
}

export function compute(data: string): string {
  function parseData(data: string): string[] {
    function splitLines(str: string): string[] {
      const result = str.split(/\r?\n/);
      if (result.at(-1) === "") {
        return result.slice(0, -1);
      } else {
        return result;
      }
    }

    return splitLines(data);
  }

  let acc = 0;
  for (const line of parseData(data)) {
    acc += line.length - replaceNumbers(line).length;
  }

  return String(acc);
}

export function solve(): void {
  try {
    const data = new TextDecoder().decode(assetData("p089_roman.txt"));

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
