import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0025.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_000);
  const expected = "4782";
  assertEquals(actual, expected);
});
