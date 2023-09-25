import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0027.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "-59231";
  assertEquals(actual, expected);
});
