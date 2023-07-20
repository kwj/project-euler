
import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0032.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "45228";
  assertEquals(actual, expected);
});
