
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0047.ts";

Deno.test("normal case 1", () => {
  const actual = compute(4);
  const expected = "134043";
  assertEquals(actual, expected);
});
