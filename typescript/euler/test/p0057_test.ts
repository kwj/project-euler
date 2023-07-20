
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0057.ts";

Deno.test("normal case 1", () => {
  const actual = compute(8);
  const expected = "1";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1_000);
  const expected = "153";
  assertEquals(actual, expected);
});
