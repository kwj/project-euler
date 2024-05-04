import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0019.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "171";
  assertEquals(actual, expected);
});
