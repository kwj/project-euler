
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0063.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "49";
  assertEquals(actual, expected);
});
