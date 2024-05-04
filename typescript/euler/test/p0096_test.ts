import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0096.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", () => {
  const data = new TextDecoder().decode(assetData("p096_sudoku.txt"));
  const actual = compute(data);
  const expected = "24702";
  assertEquals(actual, expected);
});
