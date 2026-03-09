import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0099.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", async () => {
  const data = await assetData("p099_base_exp.txt");
  const actual = compute(data);
  const expected = "709";
  assertEquals(actual, expected);
});
