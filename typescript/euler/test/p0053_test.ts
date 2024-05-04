import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0053.ts";

Deno.test("normal case 1", () => {
  const actual = compute(23, 1_000_000);
  const expected = "4";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(100, 1_000_000);
  const expected = "4075";
  assertEquals(actual, expected);
});
