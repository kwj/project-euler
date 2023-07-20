
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0071.ts";

Deno.test("normal case 1", () => {
  const actual = compute(8);
  const expected = "2";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1_000_000);
  const expected = "428570";
  assertEquals(actual, expected);
});
