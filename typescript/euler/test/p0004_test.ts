
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0004.ts";

Deno.test("normal case 1", () => {
  const actual = compute(2);
  const expected = "9009";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(3);
  const expected = "906609";
  assertEquals(actual, expected);
});
