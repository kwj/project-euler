import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0062.ts";

Deno.test("normal case 1", () => {
  const actual = compute(3);
  const expected = "41063625";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(5);
  const expected = "127035954683";
  assertEquals(actual, expected);
});
