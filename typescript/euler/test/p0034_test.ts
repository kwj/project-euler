import { assertEquals } from "@std/assert";
import { compute } from "../bin/p0034.ts";

Deno.test("normal case 1", () => {
  const actual = compute();
  const expected = "40730";
  assertEquals(actual, expected);
});
