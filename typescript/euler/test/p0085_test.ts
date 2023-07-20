
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0085.ts";

Deno.test("normal case 1", () => {
  const actual = compute(2_000_000)
  const expected = "2772";
  assertEquals(actual, expected);
});
