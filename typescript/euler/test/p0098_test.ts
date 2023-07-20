
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0098.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", () => {
  const data = new TextDecoder().decode(assetData("p098_words.txt"));
  const actual = compute(data);
  const expected = "18769";
  assertEquals(actual, expected);
});
