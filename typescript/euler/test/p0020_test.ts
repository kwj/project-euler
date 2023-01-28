
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0020.ts";

Deno.test("normal case 1", () => {
  const actual = compute(10n);
  const expected = "27";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(100n);
  const expected = "648";
  assertEquals(actual, expected);
});
