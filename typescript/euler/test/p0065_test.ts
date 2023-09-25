import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0065.ts";

Deno.test("normal case 1", () => {
  const actual = compute(10);
  const expected = "17";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(100);
  const expected = "272";
  assertEquals(actual, expected);
});
