
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0029.ts";

Deno.test("normal case 1", () => {
  const actual = compute(5);
  const expected = "15";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(100);
  const expected = "9183";
  assertEquals(actual, expected);
});
