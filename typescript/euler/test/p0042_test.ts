import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0042.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", () => {
  const data = new TextDecoder().decode(assetData("p042_words.txt"));
  const actual = compute(data);
  const expected = "162";
  assertEquals(actual, expected);
});
