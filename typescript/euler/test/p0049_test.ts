import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0049.ts";

Deno.test("normal case 1", () => {
  const actual = compute(9_999);
  const expected = "296962999629";
  assertEquals(actual, expected);
});
