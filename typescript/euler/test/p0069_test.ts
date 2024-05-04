import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0069.ts";

Deno.test("normal case 1", () => {
  const actual = compute(10);
  const expected = "6";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1_000_000);
  const expected = "510510";
  assertEquals(actual, expected);
});
