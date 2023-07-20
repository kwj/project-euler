
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0084.ts";

Deno.test("normal case 1", () => {
  const actual = compute(4, 100, 100_000);
  const expected = "101524";
  assertEquals(actual, expected);
});
