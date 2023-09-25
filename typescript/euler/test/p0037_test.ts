import { assertEquals } from "std/assert/assert_equals.ts";
import { compute } from "../bin/p0037.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "748317";
  assertEquals(actual, expected);
});
