
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0074.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_000_000, 60);
  const expected = "402";
  assertEquals(actual, expected);
});
