
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0044.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "5482660";
  assertEquals(actual, expected);
});
