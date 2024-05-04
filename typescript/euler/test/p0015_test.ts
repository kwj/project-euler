import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0015.ts";

Deno.test("normal case 1", () => {
  const actual = compute(2, 2);
  const expected = "6";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(20, 20);
  const expected = "137846528820";
  assertEquals(actual, expected);
});
