import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0042.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", async () => {
  const data = await assetData("p042_words.txt");
  const actual = compute(data);
  const expected = "162";
  assertEquals(actual, expected);
});
