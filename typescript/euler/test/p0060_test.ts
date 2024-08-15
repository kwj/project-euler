import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0060.ts";

Deno.test("normal case 1", () => {
  const actual = compute(4);
  const expected = "792";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(5);
  const expected = "26033";
  assertEquals(actual, expected);
});
