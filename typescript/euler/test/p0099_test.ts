
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0099.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", () => {
  const data = new TextDecoder().decode(assetData("p099_base_exp.txt"));
  const actual = compute(data);
  const expected = "709";
  assertEquals(actual, expected);
});
