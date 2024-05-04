import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0001.ts";

Deno.test("normal case 1", () => {
  const actual = compute(10);
  const expected = "23";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1_000);
  const expected = "233168";
  assertEquals(actual, expected);
});
