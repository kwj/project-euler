import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0079.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", () => {
  const data = new TextDecoder().decode(assetData("p079_keylog.txt"));
  const actual = compute(data);
  const expected = "73162890";
  assertEquals(actual, expected);
});
