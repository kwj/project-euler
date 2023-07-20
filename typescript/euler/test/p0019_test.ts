
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0019.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "171";
  assertEquals(actual, expected);
});
