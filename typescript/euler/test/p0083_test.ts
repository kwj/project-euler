import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0083.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", () => {
  const data = new TextDecoder().decode(assetData("p083_matrix.txt"));
  const actual = compute(data);
  const expected = "425185";
  assertEquals(actual, expected);
});
