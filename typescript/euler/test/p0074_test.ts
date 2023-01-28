
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0074.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_000_000, 60);
  const expected = "402";
  assertEquals(actual, expected);
});
