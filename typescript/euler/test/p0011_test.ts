import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0011.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "70600674";
  assertEquals(actual, expected);
});
