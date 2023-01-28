
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0092.ts";

Deno.test("normal case 1", () => {
  const actual = compute(10)
  const expected = "7";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(10_000_000);
  const expected = "8581146";
  assertEquals(actual, expected);
});
