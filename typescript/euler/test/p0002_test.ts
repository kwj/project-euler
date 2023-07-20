
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0002.ts";

Deno.test("normal case 1", () => {
  const actual = compute(100);
  const expected = "44";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(4_000_000);
  const expected = "4613732";
  assertEquals(actual, expected);
});
