import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0080.ts";

Deno.test("normal case 1", () => {
  const actual = compute(2, 100);
  const expected = "475";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(100, 100);
  const expected = "40886";
  assertEquals(actual, expected);
});
