
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0007.ts";

Deno.test("normal case 1", () => {
  const actual = compute(6);
  const expected = "13";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(10_001);
  const expected = "104743";
  assertEquals(actual, expected);
});
