
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0036.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_000_000);
  const expected = "872187";
  assertEquals(actual, expected);
});
