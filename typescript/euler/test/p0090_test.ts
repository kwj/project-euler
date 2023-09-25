import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0090.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "1217";
  assertEquals(actual, expected);
});
