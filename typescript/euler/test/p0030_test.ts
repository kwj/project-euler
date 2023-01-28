
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0030.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "443839";
  assertEquals(actual, expected);
});
