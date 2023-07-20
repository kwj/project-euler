
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0091.ts";

Deno.test("normal case 1", () => {
  const actual = compute(2, 2);
  const expected = "14";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(50, 50);
  const expected = "14234";
  assertEquals(actual, expected);
});
