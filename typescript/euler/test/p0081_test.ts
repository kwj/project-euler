import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0081.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", async () => {
  const data = await assetData("p081_matrix.txt");
  const actual = compute(Math.min, data);
  const expected = "427337";
  assertEquals(actual, expected);
});
