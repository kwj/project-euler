
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0067.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", () => {
  const data = new TextDecoder().decode(assetData("p067_triangle.txt"));
  const actual = compute(Math.max, data);
  const expected = "7273";
  assertEquals(actual, expected);
});
