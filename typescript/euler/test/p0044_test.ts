
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0044.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "5482660";
  assertEquals(actual, expected);
});
