import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0066.ts";

Deno.test("normal case 1", () => {
  const actual = compute(7);
  const expected = "5";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1_000);
  const expected = "661";
  assertEquals(actual, expected);
});
