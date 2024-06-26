import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0028.ts";

Deno.test("normal case 1", () => {
  const actual = compute(5);
  const expected = "101";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1_001);
  const expected = "669171001";
  assertEquals(actual, expected);
});
