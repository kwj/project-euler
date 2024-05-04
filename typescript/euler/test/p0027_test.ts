import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0027.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "-59231";
  assertEquals(actual, expected);
});
