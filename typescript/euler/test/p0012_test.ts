
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0012.ts";

Deno.test("normal case 1", () => {
  const actual = compute(5);
  const expected = "28";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(500);
  const expected = "76576500";
  assertEquals(actual, expected);
});
