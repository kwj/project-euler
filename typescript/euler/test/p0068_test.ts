import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0068.ts";

Deno.test("normal case 1", () => {
  const actual = compute(3);
  const expected = "432621513";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(5);
  const expected = "6531031914842725";
  assertEquals(actual, expected);
});
