import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0022.ts";
import { assetData } from "../lib/asset.ts";

Deno.test("normal case 1", async () => {
  const data = await assetData("p022_names.txt");
  const actual = compute(data);
  const expected = "871198282";
  assertEquals(actual, expected);
});
