
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0025.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_000);
  const expected = "4782";
  assertEquals(actual, expected);
});
