import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0082.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", async () => {
  const data = await assetData("p082_matrix.txt");
  const actual = compute(Math.min, data);
  const expected = "260324";
  assertEquals(actual, expected);
});
