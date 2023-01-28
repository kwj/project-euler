
import { assertEquals } from "std/testing/asserts.ts";
import { compute } from "../bin/p0073.ts";

Deno.test("normal case 1", () => {
  const actual = compute(12_000);
  const expected = "7295372";
  assertEquals(actual, expected);
});
