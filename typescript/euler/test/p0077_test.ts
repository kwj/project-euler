
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0077.ts";

Deno.test("normal case 1", () => {
  const actual = compute(4);
  const expected = "10";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(5_000);
  const expected = "71";
  assertEquals(actual, expected);
});
