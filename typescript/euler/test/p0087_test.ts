
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0087.ts";

Deno.test("normal case 1", () => {
  const actual = compute(50_000_000);
  const expected = "1097343";
  assertEquals(actual, expected);
});
