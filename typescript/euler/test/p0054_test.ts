import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0054.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", () => {
  const data = new TextDecoder().decode(assetData("p054_poker.txt"));
  const actual = compute(data);
  const expected = "376";
  assertEquals(actual, expected);
});
