import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0010.ts";

Deno.test("normal case 1", () => {
  const actual = compute(10);
  const expected = "17";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(2_000_000);
  const expected = "142913828922";
  assertEquals(actual, expected);
});
