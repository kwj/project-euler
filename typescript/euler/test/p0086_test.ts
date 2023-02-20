
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0086.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_000_000);
  const expected = "1818";
  assertEquals(actual, expected);
});