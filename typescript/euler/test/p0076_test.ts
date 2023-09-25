import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0076.ts";
import { range } from "../lib/util.ts";

Deno.test("normal case 1", () => {
  const actual = compute(range(1, 100), 100);
  const expected = "190569291";
  assertEquals(actual, expected);
});
