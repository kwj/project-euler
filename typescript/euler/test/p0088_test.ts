import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0088.ts";

Deno.test("normal case 1", () => {
  const actual = compute(6);
  const expected = "30";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(12);
  const expected = "61";
  assertEquals(actual, expected);
});

Deno.test("normal case 3", () => {
  const actual = compute(12_000);
  const expected = "7587457";
  assertEquals(actual, expected);
});
