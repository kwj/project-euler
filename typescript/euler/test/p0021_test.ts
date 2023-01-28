
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0021.ts";

Deno.test("normal case 1", () => {
  const actual = compute(10_000);
  const expected = "31626";
  assertEquals(actual, expected);
});
