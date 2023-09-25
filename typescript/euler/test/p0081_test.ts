import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0081.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", () => {
  const data = new TextDecoder().decode(assetData("p081_matrix.txt"));
  const actual = compute(Math.min, data);
  const expected = "427337";
  assertEquals(actual, expected);
});
