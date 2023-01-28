
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0005.ts";

Deno.test("normal case 1", () => {
  const actual = compute(10);
  const expected = "2520";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(20);
  const expected = "232792560";
  assertEquals(actual, expected);
});
