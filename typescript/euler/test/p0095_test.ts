import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0095.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_000_000);
  const expected = "14316";
  assertEquals(actual, expected);
});
