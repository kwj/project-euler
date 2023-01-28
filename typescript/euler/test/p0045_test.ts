
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0045.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "1533776805";
  assertEquals(actual, expected);
});
