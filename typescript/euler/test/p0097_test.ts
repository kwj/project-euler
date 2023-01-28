
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0097.ts";

Deno.test("normal case 1", () => {
  const actual = compute()
  const expected = "8739992577";
  assertEquals(actual, expected);
});
