// project euler: problem 96

/*
  [Grid]
      C0 C1 C2 C3 C4 C5 C6 C7 C8
     +--------+--------+--------+    R: Row
   R0|        |        |        |    C: Column
   R1|   B0   |   B1   |   B2   |    B: Box
   R2|        |        |        |
     +--------+--------+--------+    Cell = RxCy
   R3|        |        |        |
   R4|   B3   |   B4   |   B5   |
   R5|        |        |        |
     +--------+--------+--------+
   R6|        |        |        |
   R7|   B6   |   B7   |   B8   |
   R8|        |        |        |
     +--------+--------+--------+

  We will need the following files to run this program.
    - https://projecteuler.net/project/resources/p096_sudoku.txt
*/

import { chunk, zip } from "@std/collections";
import { cartesianProduct } from "combinatorics/mod.ts";
import { assetData } from "../lib/asset.ts";
import { maxLst } from "../lib/math.ts";
import { dedupSort, range } from "../lib/util.ts";

/* row/column/position(cell) */

// R0, R1, ..., R8
const ROW = range(0, 9).map((x) => "R" + x);

// C0, C1, ..., C8
const COL = range(0, 9).map((x) => "C" + x);

// R0C0, R0C1, ..., R8C8
const POS = [...cartesianProduct(ROW, COL)].map(([r, c]) => r + c);

/* list of cells belonging to row/column/box */

const ROW_LST = ROW.map((r) => POS.filter((pos) => pos.includes(r)));
const COL_LST = COL.map((c) => POS.filter((pos) => pos.includes(c)));
const BOX_LST = [...cartesianProduct(chunk(ROW, 3), chunk(COL, 3))].map((
  [rs, cs],
) => [...cartesianProduct(rs, cs)]).map((lst) => lst.map(([r, c]) => r + c));

/* map of cells and their corresponding row, column and box */
const groups = new Map(
  POS.map((pos) => [
    pos,
    ROW_LST.concat(COL_LST).concat(BOX_LST).filter((lst) => lst.includes(pos)),
  ]),
);

/* map of cells and their corresponding cells */
const links = new Map(
  [...groups.entries()].map(([k, v]) => [
    k,
    dedupSort(v.flat().filter((x) => x !== k)),
  ]),
);

class Grid {
  // We make a grid as Map object which values are primitive data, string.
  // It make backtracking search easily.
  grid = new Map(POS.map((pos) => [pos, "123456789"]));
  data: string;

  constructor(data: string) {
    this.data = data;
  }

  setupGrid(): boolean {
    for (const [pos, ch] of zip(POS, this.data.split(""))) {
      if (ch !== "0") {
        if (!this.decideNum(this.grid, pos, ch)) {
          return false;
        }
      }
    }

    return true;
  }

  // Tentatively determine a number to leave in the cell and remove other numbers
  decideNum(
    grid: Map<string, string>,
    pos: string,
    num: string,
  ): Map<string, string> | undefined {
    const otherNums = grid.get(pos)!.replace(num, "");
    if (otherNums.split("").every((n) => this.removeNum(grid, pos, n))) {
      return grid;
    } else {
      return undefined;
    }
  }

  // Remove the number from the candidate in the cell and update involved cells
  //
  // Note: After processing this method, the calling variable 'grid' is updated
  //       since this parameter is not copied from the original
  //       but a reference to the original.
  removeNum(grid: Map<string, string>, pos: string, num: string): boolean {
    // If the target number to remove is not already in the cell, do nothing
    if (!grid.get(pos)!.includes(num)) {
      return true;
    }
    // If there is only the target number in the cell to be removed, it is a contradiction
    if (grid.get(pos) === num) {
      return false;
    }

    grid.set(pos, grid.get(pos)!.replace(num, ""));

    // If the deletion results only one number is in the cell,
    // remove the number from the linked cells
    if (grid.get(pos)!.length === 1) {
      if (
        !links.get(pos)?.every((p) => this.removeNum(grid, p, grid.get(pos)!))
      ) {
        return false;
      }
    }

    // check row/column/box
    for (const group of groups.get(pos)!) {
      const cells = group.filter((sq) => grid.get(sq)!.includes(num));

      // If the target number doesn't remain in a belong row/column/box, it is a contradiction.
      if (cells.length === 0) {
        return false;
      }

      // If there is an only one cell which contains the removed number in a belong row/column/box,
      // the number is decided tentatively to remain in the cell
      if (cells.length === 1) {
        if (!this.decideNum(grid, cells[0], num)) {
          return false;
        }
      }
    }

    // everything is OK.
    return true;
  }

  _solve(
    grid: Map<string, string> | undefined,
  ): Map<string, string> | undefined {
    if (grid === undefined) {
      return undefined;
    }

    // an answer found
    const cells: [number, string][] = POS.map((p) => [grid.get(p)!.length, p]);
    if (cells.every((c) => c[0] === 1)) {
      return grid;
    }

    // select the cell with the smallest number of candidates and continue the search
    const [_, pos] =
      cells.filter((c) => c[0] > 1).sort((a, b) => a[0] - b[0])[0];
    for (const num of grid.get(pos)!) {
      const result = this._solve(this.decideNum(new Map(grid), pos, num));
      if (result) {
        return result;
      }
    }

    return undefined;
  }

  solve(): Map<string, string> | undefined {
    return this._solve(this.grid);
  }

  // for debug
  display(grid: Map<string, string>) {
    const values = [...grid.values()];
    const width = maxLst(values.map((s) => s.length)) + 1;
    const line_sep = [
      "-".repeat(width * 3),
      "-".repeat(width * 3),
      "-".repeat(width * 3),
    ].join("+");

    let sep_flag = 0;
    for (const row of chunk(values.map((x) => x.padStart(width, " ")), 9)) {
      console.log(chunk(row, 3).map((x) => x.join("")).join("|"));
      sep_flag += 1;
      if (sep_flag === 3 || sep_flag === 6) {
        console.log(line_sep);
      }
    }
    console.log("");

    return;
  }
}

const parseData = (data: string): string[] => {
  const splitLines = (str: string): string[] => {
    const result = str.split(/\r?\n/);
    if (result.at(-1) === "") {
      return result.slice(0, -1);
    } else {
      return result;
    }
  };

  const trim = (lst: string[]): string =>
    lst.reduce((acc, cur) => acc + cur)
      .replace(/[^0-9.]/g, "")
      .replaceAll(".", "0");

  let acc: string[] = [];
  const result: string[] = [];

  for (const line of splitLines(data)) {
    if (line.match(/^[0-9.]/) !== null) {
      acc.push(line);
    } else if (line.match(/^-/) !== null || acc.length === 0) {
      continue;
    } else {
      result.push(trim(acc));
      acc = [];
    }
  }
  if (acc.length === 9) {
    result.push(trim(acc));
  }

  if (result.some((x) => x.length !== 81)) {
    throw new Error("invalid data file");
  }

  return result;
};

export const compute = (data: string): string => {
  let acc = 0;
  for (const [idx, problem] of parseData(data).entries()) {
    const grid = new Grid(problem);
    if (!grid.setupGrid()) {
      console.log("Invalid data: Grid", idx + 1);
      continue;
    }

    const d = grid.solve();
    if (d) {
      acc += Number(d.get("R0C0")! + d.get("R0C1")! + d.get("R0C2")!);
    } else {
      console.log("No answer found: Grod", idx + 1);
    }
  }

  return String(acc);
};

export const solve = (): string => {
  const data = new TextDecoder().decode(assetData("p096_sudoku.txt"));
  return compute(data);
};
