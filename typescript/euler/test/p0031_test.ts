
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0031.ts";

Deno.test("normal case 1", () => {
  const actual = compute([1, 2, 5, 10, 20, 50, 100, 200], 200);
  const expected = "73682";
  assertEquals(actual, expected);
});
