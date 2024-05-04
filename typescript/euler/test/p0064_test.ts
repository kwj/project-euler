import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0064.ts";

Deno.test("normal case 1", () => {
  const actual = compute(13);
  const expected = "4";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(10_000);
  const expected = "1322";
  assertEquals(actual, expected);
});
