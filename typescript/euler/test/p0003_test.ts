
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0003.ts";

Deno.test("normal case 1", () => {
  const actual = compute(13195);
  const expected = "29";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(600851475143);
  const expected = "6857";
  assertEquals(actual, expected);
});
