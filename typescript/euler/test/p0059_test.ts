import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0059.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", () => {
  const data = new TextDecoder().decode(assetData("p059_cipher.txt"));
  const actual = compute(data);
  const expected = "129448";
  assertEquals(actual, expected);
});
