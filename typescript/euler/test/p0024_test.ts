
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0024.ts";
import { range } from "../lib/util.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_000_000, range(0, 10));
  const expected = "2783915460";
  assertEquals(actual, expected);
});
