
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0056.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "972";
  assertEquals(actual, expected);
});
