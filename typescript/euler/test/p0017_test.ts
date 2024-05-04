import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0017.ts";

Deno.test("normal case 1", () => {
  const actual = compute(5);
  const expected = "19";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1000);
  const expected = "21124";
  assertEquals(actual, expected);
});
