import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0043.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "16695334890";
  assertEquals(actual, expected);
});
