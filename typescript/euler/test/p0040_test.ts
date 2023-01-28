
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0040.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "210";
  assertEquals(actual, expected);
});
