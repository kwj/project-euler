
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0035.ts";

Deno.test("normal case 1", () => {
  const actual = compute(100);
  const expected = "13";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1_000_000);
  const expected = "55";
  assertEquals(actual, expected);
});
