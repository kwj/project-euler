
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0016.ts";

Deno.test("normal case 1", () => {
  const actual = compute(15n);
  const expected = "26";
  assertEquals(actual, expected);
});

Deno.test("normal case 2", () => {
  const actual = compute(1000n);
  const expected = "1366";
  assertEquals(actual, expected);
});
