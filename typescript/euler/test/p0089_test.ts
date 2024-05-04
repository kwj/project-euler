import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0089.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", () => {
  const data = new TextDecoder().decode(assetData("p089_roman.txt"));
  const actual = compute(data);
  const expected = "743";
  assertEquals(actual, expected);
});
