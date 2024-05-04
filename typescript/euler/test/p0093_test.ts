import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0093.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "1258";
  assertEquals(actual, expected);
});
