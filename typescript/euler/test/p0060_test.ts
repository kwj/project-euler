import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0060.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "26033";
  assertEquals(actual, expected);
});
