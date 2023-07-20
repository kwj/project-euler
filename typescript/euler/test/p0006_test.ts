
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0006.ts";

Deno.test("normal case 1", () => {
  const actual = compute(10);
  const expected = "2640";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(100);
  const expected = "25164150";
  assertEquals(actual, expected);
});
