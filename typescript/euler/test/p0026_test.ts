
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0026.ts";

Deno.test("normal case 1", () => {
  const actual = compute(1_000);
  const expected = "983";
  assertEquals(actual, expected);
});
