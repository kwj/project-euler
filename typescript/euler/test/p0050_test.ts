import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0050.ts";

Deno.test("normal case 1", () => {
  const actual = compute(100);
  const expected = "41";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1_000);
  const expected = "953";
  assertEquals(actual, expected);
});

Deno.test("normal case 3", () => {
  const actual = compute(1_000_000);
  const expected = "997651";
  assertEquals(actual, expected);
});
