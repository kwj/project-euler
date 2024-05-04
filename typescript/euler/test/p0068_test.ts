import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0068.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "6531031914842725";
  assertEquals(actual, expected);
});
