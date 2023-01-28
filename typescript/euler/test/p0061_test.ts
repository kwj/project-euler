
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0061.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "28684";
  assertEquals(actual, expected);
});
