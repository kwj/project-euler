
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0035.ts";

Deno.test("normal case 1", () => {
  const actual = compute(100);
  const expected = "13";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1_000_000);
  const expected = "55";
  assertEquals(actual, expected);
});
